-- | 
{-# language ConstraintKinds #-}
{-# language RecordWildCards #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}

module System.Nix.Internal.ContentAdressed where

import qualified Relude.Unsafe as Unsafe
import qualified Text.Show
import           System.Nix.Internal.Hash
import           System.Nix.Internal.Base
import qualified System.Nix.Internal.Base32    as Nix.Base32
import System.Nix.Internal.StorePath
import           Crypto.Hash                    ( SHA256
                                                , Digest
                                                )

data StoreReferences = StoreReferences
  { storeReferences_others :: !StorePathSet
  , storeReferences_self :: !Bool
  , storeReferences_empty :: !Bool
  , storeReferences_size :: !Int
  }

data FixedOutputInfo hashType = FixedOutputInfo
  { fixedOutputInfo_hash :: !(Digest hashType)
  , fixedOutputInfo_references :: !StorePathSet
  }

data TextInfo hashType = TextInfo
  { textInfo_textHash :: !(TextHash hashType)
  , textInfo_references :: !StorePathSet
  }

data TextHash hashType = TextHash
  {
   textHash_hash :: !(Digest hashType)  
  }

data FixedOutputHash hashType = FixedOutputHash
  { fixedOutputHash_method :: !FileIngestionMethod
  , fixedOutputHash_hash :: !(Digest hashType)
  }

data IngestionMethod = FileIngestionMethod !FileIngestionMethod
                     | TextIngestionMethod

data FileIngestionMethod = Flat
                         | FileRecursive
