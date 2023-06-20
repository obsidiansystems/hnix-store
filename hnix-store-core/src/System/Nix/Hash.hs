{-|
Description : Cryptographic hashes for hnix-store.
-}
module System.Nix.Hash
  ( Hash.mkStorePathHash
  , Hash.NamedAlgo(..)
  , Hash.HashAlgo(..)
  , Hash.SomeHashAlgo
  , Hash.algoToText
  , Hash.textToAlgo
  , Hash.SomeNamedDigest(..)

  , Hash.mkNamedDigest

  , Base.BaseEncoding(..)
  , Hash.encodeDigestWith
  , Hash.decodeDigestWith
  )
where

import qualified System.Nix.Internal.Hash      as Hash
import qualified System.Nix.Internal.Base      as Base
