{-# language AllowAmbiguousTypes #-}
{-# language KindSignatures      #-}
{-# language GADTs #-}
{-# language RankNTypes          #-}
{-# language ScopedTypeVariables #-}
{-# language DataKinds           #-}
{-# language RecordWildCards     #-}
{-# language LiberalTypeSynonyms #-}

module System.Nix.Store.Remote.GADT
  ( RepairFlag
  , CheckFlag
  , SubstituteFlag
  , StoreRequest(..)
  )
where

import Prelude hiding (bool, put, get)

import Data.ByteString.Lazy qualified as BSL
import Data.HashSet (HashSet)
import Data.Set (Set)
import Data.Some
import Nix.Derivation (Derivation)
import System.Nix.Build (BuildMode, BuildResult)
import System.Nix.DerivedPath
import System.Nix.Hash (HashAlgo)
import System.Nix.Nar (NarSource)
import System.Nix.StorePath (StorePath, StorePathName, StorePathHashPart)
import System.Nix.ValidPathInfo (ValidPathInfo)

type RepairFlag = Bool
type CheckFlag = Bool
type SubstituteFlag = Bool

data StoreRequest :: Type -> Type where
  -- | Pack `Nar` and add it to the store.
  AddToStore
    :: StorePathName        -- ^ Name part of the newly created `StorePath`
    -> Bool                 -- ^ Add target directory recursively
    -> Some HashAlgo
    -> (forall m. MonadIO m => NarSource m) -- ^ provide nar stream
    -> RepairFlag           -- ^ Only used by local store backend
    -> StoreRequest StorePath

  -- | Add text to store.
  --
  -- Reference accepts repair but only uses it
  -- to throw error in case of remote talking to nix-daemon.
  AddTextToStore
    :: Text         -- ^ Name of the text
    -> Text         -- ^ Actual text to add
    -> HashSet StorePath -- ^ Set of `StorePath`s that the added text references
    -> RepairFlag   -- ^ Repair flag, must be `False` in case of remote backend
    -> StoreRequest StorePath

  AddSignatures
    :: StorePath
    -> [BSL.ByteString]
    -> StoreRequest ()

  -- | Add temporary garbage collector root.
  --
  -- This root is removed as soon as the client exits.
  AddIndirectRoot
    :: StorePath
    -> StoreRequest ()

  AddTempRoot
    :: StorePath
    -> StoreRequest ()

  -- | Build paths if they are an actual derivations.
  --
  -- If derivation output paths are already valid, do nothing.
  BuildPaths
    :: Set DerivedPath
    -> BuildMode
    -> StoreRequest ()

  BuildDerivation
    :: StorePath
    -> Derivation StorePath Text
    -> BuildMode
    -> StoreRequest BuildResult

  EnsurePath
    :: StorePath
    -> StoreRequest ()

  -- | Find garbage collector roots.
  FindRoots
    :: StoreRequest (Map BSL.ByteString StorePath)

  IsValidPath
    :: StorePath
    -> StoreRequest Bool

  -- | Query valid paths from set, optionally try to use substitutes.
  QueryValidPaths
    :: HashSet StorePath -- ^ Set of `StorePath`s to query
    -> SubstituteFlag -- ^ Try substituting missing paths when `True`
    -> StoreRequest (HashSet StorePath)

  QueryAllValidPaths
    :: StoreRequest (HashSet StorePath)

  QuerySubstitutablePaths
    :: HashSet StorePath
    -> StoreRequest (HashSet StorePath)

  QueryPathInfo
    :: StorePath
    -> StoreRequest (Maybe ValidPathInfo)

  QueryReferrers
    :: StorePath
    -> StoreRequest (HashSet StorePath)

  QueryValidDerivers
    :: StorePath
    -> StoreRequest (HashSet StorePath)

  QueryDerivationOutputs
    :: StorePath
    -> StoreRequest (HashSet StorePath)

  QueryDerivationOutputNames
    :: StorePath
    -> StoreRequest (HashSet StorePathName)

  QueryPathFromHashPart
    :: StorePathHashPart
    -> StoreRequest StorePath

  QueryMissing
    :: Set DerivedPath
    -> StoreRequest
      ( HashSet StorePath -- Paths that will be built
      , HashSet StorePath -- Paths that have substitutes
      , HashSet StorePath -- Unknown paths
      , Integer -- Download size
      , Integer -- Nar size?
      )

  OptimiseStore
    :: StoreRequest ()

  SyncWithGC
    :: StoreRequest ()

  -- returns True on errors
  VerifyStore
    :: CheckFlag
    -> RepairFlag
    -> StoreRequest Bool

