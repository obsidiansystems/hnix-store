{-|
Description : Utilities for packing stuff
Maintainer  : srk <srk@48.io>
|-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language RecordWildCards     #-}
module System.Nix.Store.Remote.Binary
  ( Get
  , Put
  , PutM
  , Serializer(..)
  , mapPrismSerializer
  , mapIsoSerializer
  -- *
  , int
  , bool
  , enum
  , time
  , maybe
  , list
  , set
  , hashSet
  , tup
  , lazyMap
  , strictMap
  , lazyByteStringLen
  , byteStringLen
  , text
  , maybeText
  -- *
  , protoVersion
  , field
  , errorInfo
  -- *
  , path
  , derivationOutput
  , derivation
  , buildResult
  , pathMetadata
  -- -- *
  -- , storeRequest
  ) where

import qualified Prelude
import           Prelude                 hiding (bool, map, maybe, put, get, putText )

import qualified Data.Binary.Get               as B
import qualified Data.Binary.Put               as B
import qualified Data.ByteString.Lazy          as BSL
--import           Data.Some
import           Data.Time
import           Data.Time.Clock.POSIX

import           Nix.Derivation hiding (path)

import           System.Nix.Build
--import           System.Nix.Hash                ( BaseEncoding(NixBase32)
--                                                )
import           System.Nix.StorePath
import           System.Nix.Hash                ( SomeNamedDigest(..)
                                                , BaseEncoding(NixBase32)
                                                , decodeDigestWith
                                                )
import           System.Nix.StorePathMetadata
import qualified System.Nix.Store.Remote.Parsers
import           System.Nix.Store.Remote.TextConv
--import           System.Nix.Store.Remote.GADT as R
--import qualified System.Nix.Store.Remote.Protocol as P
import           System.Nix.Store.Remote.Protocol hiding ( WorkerOp(..), protoVersion )
import           Crypto.Hash                    ( SHA256 )

import           Data.Bits
import qualified Data.HashSet
import qualified Data.Map.Lazy
import qualified Data.Map.Strict
import qualified Data.Set

type Get r = ReaderT r B.Get
type PutM r = ReaderT r B.PutM
type Put r = PutM r ()

data Serializer r a = Serializer
  { get :: Get r a
  , put :: a -> Put r
  }

mapIsoSerializer
  :: (a -> b)
  -> (b -> a)
  -> (Serializer r a -> Serializer r b)
mapIsoSerializer f g s = Serializer
  { get = f <$> get s
  , put = put s . g
  }

mapPrismSerializer
  :: (a -> Either String b)
  -> (b -> a)
  -> (Serializer r a -> Serializer r b)
mapPrismSerializer f g s = Serializer
  { get = either fail pure . f =<< get s
  , put = put s . g
  }

int :: Integral a => Serializer r a
int = Serializer
  { get = fromIntegral <$> lift B.getWord64le
  , put = lift . B.putWord64le . fromIntegral
  }

bool :: Serializer r Bool
bool = mapPrismSerializer
  (\case
      0 -> Right False
      1 -> Right True
      _ -> Left "number other than 1 and 0 is illegal bool"
  )
  (\case
    True  -> 1 :: Word8
    False -> 0 :: Word8)
  int

enum :: Enum e => Serializer r e
enum = mapIsoSerializer (toEnum) fromEnum int

time :: Serializer r UTCTime
time = mapPrismSerializer (Right . posixSecondsToUTCTime) utcTimeToPOSIXSeconds enum

maybe :: Serializer r a -> Serializer r (Maybe a)
maybe a = Serializer
  { get = do
      n :: Word8 <- get int
      case n of
        0 -> pure Nothing
        1 -> Just <$> get a
        _ -> fail "invalid tag for maybe"
  , put = \case
      Nothing -> put int (0 :: Word8)
      Just x -> put int (1 :: Word8) >> put a x
  }

list :: Serializer r a -> Serializer r [a]
list a = Serializer
  { get = do
      count <- get int
      replicateM count $ get a
  , put = \xs -> do
      put int (length xs)
      mapM_ (put a) xs
  }

set :: Ord a => Serializer r a -> Serializer r (Set a)
set = mapIsoSerializer Data.Set.fromList toList . list

hashSet :: (Eq a, Hashable a) => Serializer r a -> Serializer r (HashSet a)
hashSet = mapIsoSerializer Data.HashSet.fromList toList . list

tup :: Serializer r a -> Serializer r b -> Serializer r (a, b)
tup a b = Serializer
  { get = liftA2 (,) (get a) (get b)
  , put = \(x, y) -> do
      put a x
      put b y
  }

lazyMap :: Ord k => Serializer r k -> Serializer r v -> Serializer r (Map k v)
lazyMap k v = mapIsoSerializer Data.Map.Lazy.fromList Data.Map.Lazy.toList $
  list $ tup k v

strictMap :: Ord k => Serializer r k -> Serializer r v -> Serializer r (Map k v)
strictMap k v = mapIsoSerializer Data.Map.Strict.fromList Data.Map.Strict.toList $
  list $ tup k v

lazyByteStringLen :: Serializer r BSL.ByteString
lazyByteStringLen = Serializer
  { get = do
      len <- get int
      st  <- lift $ B.getLazyByteString len
      let unpad x = replicateM x B.getWord8
      when (len `mod` 8 /= 0) $ do
        pads <- lift $ unpad $ fromIntegral $ 8 - (len `mod` 8)
        unless (all (== 0) pads) $ fail $ "No zeroes" <> show (st, len, pads)
      pure st
  , put = \x -> do
      let len :: Int
          len = fromIntegral $ BSL.length x
          pad count = replicateM_ count (lift $ B.putWord8 0)
      put int len
      lift $ B.putLazyByteString x
      when (len `mod` 8 /= 0) $ pad $ 8 - (len `mod` 8)
  }

byteStringLen :: Serializer r ByteString
byteStringLen = Serializer
  { get = toStrict <$> get lazyByteStringLen
  , put = put lazyByteStringLen . toLazy
  }

text :: Serializer r Text
text = mapPrismSerializer (Right . bslToText) textToBSL lazyByteStringLen

maybeText :: Serializer r (Maybe Text)
maybeText = mapIsoSerializer
  (\case "" -> Nothing; t -> Just t)
  (Prelude.maybe "" id)
  text

protoVersion :: Serializer r ProtoVersion
protoVersion = Serializer
  { get = do
      v :: Int32 <- get int
      pure ProtoVersion
        { protoVersion_major = fromIntegral $ shiftR v 8
        , protoVersion_minor = fromIntegral $ v .&. 0x00FF
        }
  , put = \p ->
      put int $
        (shiftL (fromIntegral $ protoVersion_major p :: Word32) 8)
        .|. fromIntegral (protoVersion_minor p)
  }

field :: Serializer r Field
field = Serializer
  { get = do
      (typ :: Int8) <- get int
      case typ of
        0 -> LogInt <$> get int
        1 -> LogStr <$> get byteStringLen
        x -> fail $ "Unknown log type: " <> show x
  , put = \case
      LogInt n -> do
        put int (0 :: Int8)
        put int n
      LogStr s -> do
        put int (1 :: Int8)
        put byteStringLen s
  }

errorInfo :: Serializer r ErrorInfo
errorInfo = undefined

path :: HasStoreDir r => Serializer r StorePath
path = Serializer
  { get = do
      sd <- getStoreDir
      x <- parsePath sd <$> get byteStringLen
      case x of
        Right v -> pure v
        _ -> fail "invalid path"
  , put = \p -> do
      sd <- getStoreDir
      put byteStringLen $ storePathToRawFilePath sd $ p
  }

-- TODO dedup
maybePath :: HasStoreDir r => Serializer r (Maybe StorePath)
maybePath = Serializer
  { get = do
      sd <- getStoreDir
      b <- get byteStringLen
      if b == ""
        then pure Nothing
        else do
          x <- parsePath sd <$> get byteStringLen
          case x of
            Right v -> pure $ Just v
            _ -> fail "invalid path"
  , put = \case
      Nothing -> put byteStringLen ""
      Just p -> do
        sd <- getStoreDir
        put byteStringLen $ storePathToRawFilePath sd $ p
  }

derivationOutput :: HasStoreDir r => Serializer r (DerivationOutput StorePath Text)
derivationOutput = Serializer
  { get = DerivationOutput
      <$> get path
      <*> get text
      <*> get text
  , put = \(DerivationOutput a b c) -> do
      put path a
      put text b
      put text c
  }

derivation :: HasStoreDir r => Serializer r (Derivation StorePath Text)
derivation = Serializer
  { get = do
      outputs <- get (strictMap text derivationOutput)

      inputSrcs <- get (set path)
      let inputDrvs = error "This is BasicDerivation, we need to change types"
      platform <- get text
      builder <- get text
      args <- fromList <$> get (list text)

      env <- get (strictMap text text)
      pure $ Derivation{..}
  , put = \Derivation{..} -> do
      put (strictMap text derivationOutput) outputs

      put (set path) inputSrcs
      put text platform
      put text builder
      put (list text) $ toList args

      put (strictMap text text) env
  }

buildResult :: HasStoreDir r => Serializer r BuildResult
buildResult = Serializer
  { get = BuildResult
      <$> get enum
      <*> (do
        s <- bsToText <$> get byteStringLen
        pure $ if s == "" then Nothing else Just s)
      <*> get int
      <*> get bool
      <*> get time
      <*> get time
  , put = \(BuildResult a b c d e f) -> do
      put enum a
      Prelude.maybe (pure ()) (put byteStringLen . textToBS) b
      put int c
      put bool d
      put time e
      put time f
  }

pathMetadata :: HasStoreDir r => Serializer r StorePathMetadata
pathMetadata = Serializer
  { get = do
      deriverPath <- get maybePath

      narHashText <- decodeUtf8 <$> get byteStringLen
      let
        narHash =
          case
            decodeDigestWith @SHA256 NixBase32 narHashText
            of
            Left  e -> error $ fromString e
            Right x -> SomeDigest x

      references       <- get $ hashSet path
      registrationTime <- get time
      narBytes         <- Just <$> get int
      ultimate         <- get bool

      _sigStrings      <- (fmap . fmap) bsToText $ get $ list byteStringLen
      caString         <- get byteStringLen

      let
          -- XXX: signatures need pubkey from config
          sigs = Data.Set.empty

          contentAddressableAddress =
            case
              System.Nix.Store.Remote.Parsers.parseContentAddressableAddress caString
              of
              Left  e -> error $ fromString e
              Right x -> Just x

          trust = if ultimate then BuiltLocally else BuiltElsewhere

      pure $ StorePathMetadata{..}
  , put = undefined -- TODO
  }

{-
storeRequest :: Serializer r (Some StoreRequest)
storeRequest = Serializer
  { get = undefined -- TODO
  , put = \(Some f) -> case f of
      R.AddToStore (Proxy :: Proxy a) name _source recursive _repair -> do
        put text $ System.Nix.StorePath.unStorePathName name
        put bool $ not $ System.Nix.Hash.algoName @a == "sha256" && recursive
        put bool recursive
        put text $ System.Nix.Hash.algoName @a
  }
-}
