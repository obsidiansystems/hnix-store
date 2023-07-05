{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Description : Cryptographic hashing interface for hnix-store, on top
              of the cryptohash family of libraries.
-}

module System.Nix.Hash
  ( mkStorePathHash
  , HashAlgo(..)
  , NamedAlgo(..)
  , algoToText
  , textToAlgo
  , mkNamedDigest
  , Base.BaseEncoding(..)
  , encodeDigestWith
  , decodeDigestWith
  , module Base
  )
where

import qualified System.Nix.Base as Base

import Crypto.Hash qualified as C
import Data.ByteArray
import Data.ByteString qualified as BS
import Data.Constraint.Extras
import Data.Constraint.Extras.TH
import Data.Dependent.Sum
import Data.Some
import Data.Text qualified as T
import System.Nix.Base
import System.Nix.Truncation

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

algoToText :: forall t. HashAlgo t -> Text
algoToText x = has @NamedAlgo x (algoName @t)

hashAlgoValue :: HashAlgo a -> a
hashAlgoValue = \case
  HashAlgo_MD5 -> C.MD5
  HashAlgo_SHA1 -> C.SHA1
  HashAlgo_SHA256 -> C.SHA256
  HashAlgo_SHA512 -> C.SHA512

textToAlgo :: Text -> Either String (Some HashAlgo)
textToAlgo = \case
    "md5"    -> Right $ Some HashAlgo_MD5
    "sha1"   -> Right $ Some HashAlgo_SHA1
    "sha256" -> Right $ Some HashAlgo_SHA256
    "sha512" -> Right $ Some HashAlgo_SHA512
    name     -> Left $ "Unknown hash name: " <> toString name

mkNamedDigest :: Text -> Text -> Either String (DSum HashAlgo C.Digest)
mkNamedDigest name sriHash =
  let (sriName, h) = T.breakOnEnd "-" sriHash in
    if sriName == "" || sriName == name <> "-"
    then mkDigest h
    else Left $ toString $ "SRI hash method " <> sriName <> " does not match the required hash type " <> name
 where
  mkDigest h = textToAlgo name >>= \(Some a) -> has @C.HashAlgorithm a $ fmap (a :=>) $ decodeGo a h
  decodeGo :: C.HashAlgorithm a => HashAlgo a -> Text -> Either String (C.Digest a)
  decodeGo a h = if
    | size == base16Len -> decodeDigestWith Base16 h
    | size == base32Len -> decodeDigestWith NixBase32 h
    | size == base64Len -> decodeDigestWith Base64 h
    | otherwise -> Left $ toString sriHash <> " is not a valid " <> toString name <> " hash. Its length (" <> show size <> ") does not match any of " <> show [base16Len, base32Len, base64Len]
   where
    size = T.length h
    hsize = C.hashDigestSize (hashAlgoValue a)
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

deriveArgDict ''HashAlgo
