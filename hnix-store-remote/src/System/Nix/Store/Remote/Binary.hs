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
  , trustedFlag
  , protoVersion
  , field
  , errorInfo
  -- *
  , someHashAlgo
  , storePathName
  , storePathHashPart
  , path
  , maybePath
  , derivationOutput
  , derivation
  , buildResult
  , pathMetadata
  -- -- *
  -- , storeRequest
  ) where

import qualified Prelude
import Prelude                 hiding (bool, map, maybe, put, get, putText, putBS)

import Crypto.Hash qualified as C
import Data.Binary.Get qualified as B
import Data.Binary.Put qualified as B
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Constraint.Extras
import Data.Dependent.Sum
import Data.HashSet qualified
import Data.Map.Lazy qualified
import Data.Map.Strict qualified
import Data.Set qualified
import Data.Some
import Data.Time
import Data.Time.Clock.POSIX
import Nix.Derivation hiding (path)
import System.Nix.Build
import System.Nix.ContentAddress
import System.Nix.Hash
import System.Nix.Store.Remote.Protocol hiding ( WorkerOp(..), protoVersion )
import System.Nix.Store.Remote.TextConv
import System.Nix.StorePath hiding ( storePathName )
import System.Nix.ValidPathInfo

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

ultimate_ :: Serializer r Ultimate
ultimate_ = mapIsoSerializer
  (\case False -> BuiltElsewhere; True -> BuiltLocally)
  (\case BuiltElsewhere -> False; BuiltLocally -> True)
  bool

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

someHashAlgo :: Serializer r (Some HashAlgo)
someHashAlgo = mapPrismSerializer textToAlgo (foldSome algoToText) text

-- TODO validate
storePathName :: Serializer r StorePathName
storePathName = mapIsoSerializer StorePathName unStorePathName text

storePathHashPart :: Serializer r StorePathHashPart
storePathHashPart = mapIsoSerializer coerce coerce $
  mapPrismSerializer (decodeWith NixBase32) (encodeWith NixBase32) $
  text

digest :: forall a r. C.HashAlgorithm a => BaseEncoding -> Serializer r (C.Digest a)
digest base = mapIsoSerializer coerce coerce $
  mapPrismSerializer (decodeDigestWith @a base) (encodeDigestWith base) $
  text

data TrustedFlag = Trusted | NotTrusted

trustedFlag :: Serializer r (Maybe TrustedFlag)
trustedFlag = Serializer
  { get = do
      n :: Word8 <- get int
      case n of
        0 -> return $ Nothing
        1 -> return $ Just Trusted
        2 -> return $ Just NotTrusted
        _ -> fail $ "Invalid trusted status from remote: " <> show n
  , put = \n -> put int $ case n of
      Nothing -> 0 :: Word8
      Just Trusted -> 1
      Just NotTrusted -> 2
  }

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

pathMetadata :: HasStoreDir r => Serializer r ValidPathInfo
pathMetadata = Serializer
  { get = do
      deriverPath <- get maybePath

      narHash <- get $ digest Base16
      references <- get $ hashSet path
      registrationTime <- get time
      narBytes <- (\case
                      0 -> Nothing
                      size -> Just size) <$> get int
      ultimate <- get ultimate_

      _sigStrings <- (fmap . fmap) bsToText $ get $ list byteStringLen
      contentAddress <- Just <$> get contentAddressS <|> pure Nothing

      let
          -- XXX: signatures need pubkey from config
          sigs = Data.Set.empty
      pure $ ValidPathInfo{..}
  , put = \x -> do
      put maybePath $ deriverPath x
      put (digest NixBase32) $ narHash x
      put (hashSet path) $ references x
      put time $ registrationTime x
      put int $ Prelude.maybe 0 id $ narBytes x
      put ultimate_ $ ultimate x
      put (hashSet text) $ Data.HashSet.empty
      case contentAddress x of
        Nothing -> pure ()
        Just ca -> put contentAddressS ca
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

string :: ByteString -> Get r ByteString
string x = lift $ do
  s <- B.getByteString (BS.length x)
  guard (s == x)
  return s

putBS :: ByteString -> Put r
putBS s = lift (B.putByteString s)

fileIngestionMethodS :: Serializer r FileIngestionMethod
fileIngestionMethodS = Serializer
  { get = (FileRecursive <$ string "r:") <|> pure Flat
  , put = \case
      FileRecursive -> putBS "r:"
      Flat -> pure ()
  }

contentAddressMethodS :: Serializer r ContentAddressMethod
contentAddressMethodS = Serializer
  { get = (TextIngestionMethod <$ string "text:")
      <|> (FileIngestionMethod <$> (string "fixed:" *> get fileIngestionMethodS))
  , put = \case
    TextIngestionMethod -> putBS "text:"
    FileIngestionMethod s -> putBS "fixed:" >> put fileIngestionMethodS s
  }

hashAlgoS :: Serializer r (Some HashAlgo)
hashAlgoS = Serializer
  { get = Some HashAlgo_MD5 <$ string "md5"
      <|> Some HashAlgo_SHA1 <$ string "sha1"
      <|> Some HashAlgo_SHA256 <$ string "sha256"
      <|> Some HashAlgo_SHA512 <$ string "sha512"
  , put = \case
      Some HashAlgo_MD5 -> putBS "md5"
      Some HashAlgo_SHA1 -> putBS "sha1"
      Some HashAlgo_SHA256 -> putBS "sha256"
      Some HashAlgo_SHA512 -> putBS "sha512"
  }

contentAddressS :: Serializer r ContentAddress
contentAddressS = Serializer
  { get = do
      method <- get contentAddressMethodS
      Some algo <- get hashAlgoS
      d <- has @(C.HashAlgorithm) algo $
        get (digest NixBase32)
      return $ ContentAddress method (algo :=> d)
  , put = \(ContentAddress method (algo :=> d)) -> do
      put contentAddressMethodS method
      has @(C.HashAlgorithm) algo $
        put hashAlgoS (Some algo) >> put (digest NixBase32) d
  }
