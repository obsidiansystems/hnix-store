{-# language AllowAmbiguousTypes #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes          #-}

module System.Nix.Store.Remote.Builders
  ( buildContentAddress
  )
where

import Crypto.Hash ( Digest )
import Data.Dependent.Sum
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder ( Builder )
import Data.Text.Lazy.Builder qualified as TL
import System.Nix.Hash

-- | Marshall `ContentAddressableAddress` to `Text`
-- in form suitable for remote protocol usage.
buildContentAddress :: ContentAddress -> TL.Text
buildContentAddress =
  TL.toLazyText . contentAddressBuilder

contentAddressBuilder :: ContentAddress -> Builder
contentAddressBuilder (ContentAddress method hash) = case method of
  TextIngestionMethod -> "text:" <> digestBuilder hash
  FileIngestionMethod r -> "fixed:"
    <> fileIngestionMethodBuilder r
    <> digestBuilder hash

fileIngestionMethodBuilder :: FileIngestionMethod -> Builder
fileIngestionMethodBuilder = \case
  Flat -> ""
  FileRecursive -> "r:"

digestBuilder :: DSum HashAlgo Digest -> Builder
digestBuilder (a :=> d) =
  TL.fromText (algoToText a)
  <> TL.fromText (encodeDigestWith NixBase32 d)
