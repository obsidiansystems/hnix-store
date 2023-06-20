{-|
Description : Cryptographic hashing interface for hnix-store, on top
              of the cryptohash family of libraries.
-}
{-# language AllowAmbiguousTypes #-}
{-# language GADTs               #-}
{-# language TypeFamilies        #-}
{-# language ScopedTypeVariables #-}
{-# language DataKinds           #-}
{-# language ExistentialQuantification #-}
{-# language CPP #-}

module System.Nix.Internal.Hash
  ( NamedAlgo(..)
  , HashAlgo(..)
  , SomeHashAlgo
  , algoToText
  , textToAlgo
  , SomeNamedDigest(..)
  , mkNamedDigest
  , encodeDigestWith
  , decodeDigestWith
  , mkStorePathHash
  )
where

import qualified Text.Show
import qualified Crypto.Hash            as C
import qualified Data.ByteString        as BS
import           Data.Some
import qualified Data.Text              as T
import           System.Nix.Internal.Base
import           Data.ByteArray
import           System.Nix.Internal.Truncation

-- | A 'HashAlgorithm' with a canonical name, for serialization
-- purposes (e.g. SRI hashes)
class C.HashAlgorithm a => NamedAlgo a where
  algoName :: Text

instance NamedAlgo C.MD5 where
  algoName = "md5"

instance NamedAlgo C.SHA1 where
  algoName = "sha1"

instance NamedAlgo C.SHA256 where
  algoName = "sha256"

instance NamedAlgo C.SHA512 where
  algoName = "sha512"

data HashAlgo :: Type -> Type where
  HashAlgo_MD5 :: HashAlgo C.MD5
  HashAlgo_SHA1 :: HashAlgo C.SHA1
  HashAlgo_SHA256 :: HashAlgo C.SHA256
  HashAlgo_SHA512 :: HashAlgo C.SHA512

type SomeHashAlgo = Some HashAlgo

algoToText :: forall t. HashAlgo t -> Text
algoToText = \case
  HashAlgo_MD5 -> algoName @t
  HashAlgo_SHA1 -> algoName @t
  HashAlgo_SHA256 -> algoName @t
  HashAlgo_SHA512 -> algoName @t

textToAlgo :: Text -> Either String SomeHashAlgo
textToAlgo = \case
    "md5"    -> Right $ Some HashAlgo_MD5
    "sha1"   -> Right $ Some HashAlgo_SHA1
    "sha256" -> Right $ Some HashAlgo_SHA256
    "sha512" -> Right $ Some HashAlgo_SHA512
    name     -> Left $ "Unknown hash name: " <> toString name

-- | A digest whose 'NamedAlgo' is not known at compile time.
data SomeNamedDigest = forall a . NamedAlgo a => SomeDigest (C.Digest a)

instance Show SomeNamedDigest where
  show sd = case sd of
    SomeDigest (digest :: C.Digest hashType) -> toString $ "SomeDigest " <> algoName @hashType <> ":" <> encodeDigestWith NixBase32 digest

mkNamedDigest :: Text -> Text -> Either String SomeNamedDigest
mkNamedDigest name sriHash =
  let (sriName, h) = T.breakOnEnd "-" sriHash in
    if sriName == "" || sriName == name <> "-"
    then mkDigest h
    else Left $ toString $ "Sri hash method " <> sriName <> " does not match the required hash type " <> name
 where
  mkDigest h = textToAlgo name >>= \(Some a) -> case a of
    HashAlgo_MD5    -> SomeDigest <$> decodeGo C.MD5    h
    HashAlgo_SHA1   -> SomeDigest <$> decodeGo C.SHA1   h
    HashAlgo_SHA256 -> SomeDigest <$> decodeGo C.SHA256 h
    HashAlgo_SHA512 -> SomeDigest <$> decodeGo C.SHA512 h
  decodeGo :: forall a . NamedAlgo a => a -> Text -> Either String (C.Digest a)
  decodeGo a h
    | size == base16Len = decodeDigestWith Base16 h
    | size == base32Len = decodeDigestWith NixBase32 h
    | size == base64Len = decodeDigestWith Base64 h
    | otherwise = Left $ toString sriHash <> " is not a valid " <> toString name <> " hash. Its length (" <> show size <> ") does not match any of " <> show [base16Len, base32Len, base64Len]
   where
    size = T.length h
    hsize = C.hashDigestSize a
    base16Len = hsize * 2
    base32Len = ((hsize * 8 - 1) `div` 5) + 1;
    base64Len = ((4 * hsize `div` 3) + 3) `div` 4 * 4;


mkStorePathHash :: forall a . C.HashAlgorithm a => BS.ByteString -> BS.ByteString
mkStorePathHash bs =
  truncateInNixWay 20 $ convert $ C.hash @BS.ByteString @a bs

-- | Take BaseEncoding type of the output -> take the Digeest as input -> encode Digest
encodeDigestWith :: BaseEncoding -> C.Digest a -> T.Text
encodeDigestWith b = encodeWith b . convert


-- | Take BaseEncoding type of the input -> take the input itself -> decodeBase into Digest
decodeDigestWith :: C.HashAlgorithm a => BaseEncoding -> T.Text -> Either String (C.Digest a)
decodeDigestWith b x =
  do
    bs <- decodeWith b x
    let
      toEither =
        maybeToRight
          ("Cryptonite was not able to convert '(ByteString -> Digest a)' for: '" <> show bs <>"'.")
    (toEither . C.digestFromByteString) bs
