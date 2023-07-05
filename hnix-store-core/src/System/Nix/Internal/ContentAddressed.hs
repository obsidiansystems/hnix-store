{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module System.Nix.Internal.ContentAddressed where

import qualified Relude.Unsafe as Unsafe
import Data.Constraint.Extras.TH
import Data.Dependent.Sum
import qualified Text.Show
import System.Nix.Internal.Hash
import System.Nix.Internal.Base
import qualified System.Nix.Internal.Base32 as Nix.Base32
import System.Nix.Internal.StorePath
import Crypto.Hash ( SHA256, Digest )

data StoreReferences = StoreReferences
  { storeReferences_others :: !StorePathSet
  , storeReferences_self :: !Bool
  , storeReferences_empty :: !Bool
  , storeReferences_size :: !Int
  }

data ContentAddress = ContentAddress (DSum ContentAddressMethod Digest)

data ContentAddressWithReferences = ContentAddressWithReferences
  { contentAddressWithReferences_hash :: !ContentAddress
  , contentAddressWithReferences_references :: !StorePathSet
  }

data IngestionMethod = FileIngestionMethod !FileIngestionMethod
                     | TextIngestionMethod

data FileIngestionMethod = Flat
                         | FileRecursive

data ContentAddressMethod :: * -> * where
  ContentAddressMethod :: IngestionMethod -> HashAlgo a -> ContentAddressMethod a

deriveArgDict ''ContentAddressMethod
