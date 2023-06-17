{-# language AllowAmbiguousTypes #-}
{-# language KindSignatures      #-}
{-# language RankNTypes          #-}
{-# language ScopedTypeVariables #-}
{-# language DataKinds           #-}
{-# language RecordWildCards     #-}
{-# language LiberalTypeSynonyms #-}

module System.Nix.Store.Remote
  ( addToStore
  , addTextToStore
  , addSignatures
  , addIndirectRoot
  , addTempRoot
  , buildPaths
  , buildDerivation
  , ensurePath
  , findRoots
  , isValidPathUncached
  , queryValidPaths
  , queryAllValidPaths
  , querySubstitutablePaths
  , queryPathInfoUncached
  , queryReferrers
  , queryValidDerivers
  , queryDerivationOutputs
  , queryDerivationOutputNames
  , queryPathFromHashPart
  , queryMissing
  , optimiseStore
  , runStore
  , syncWithGC
  , verifyStore
  , module System.Nix.Store.Remote.Types
  )
where

import           Prelude                 hiding ( bool, put, get )
import qualified Data.ByteString.Lazy          as BSL

import           Nix.Derivation                 ( Derivation )
import           System.Nix.Build               ( BuildMode
                                                , BuildResult
                                                )
import           System.Nix.Hash                ( NamedAlgo(..)
                                                , BaseEncoding(NixBase32)
                                                )
import           System.Nix.StorePath           ( StorePath
                                                , StorePathName
                                                , StorePathSet
                                                , StorePathHashPart
                                                )
import           System.Nix.Internal.Base       ( encodeWith )

import qualified Data.Binary.Put
import qualified Data.Map.Strict

import qualified System.Nix.StorePath
import           System.Nix.StorePathMetadata  ( StorePathMetadata )

import           System.Nix.Store.Remote.Binary
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Socket
import           System.Nix.Nar                 ( NarSource )

type RepairFlag = Bool
type CheckFlag = Bool
type SubstituteFlag = Bool

-- | Pack `Nar` and add it to the store.
addToStore
  :: forall a
   . (NamedAlgo a)
  => StorePathName        -- ^ Name part of the newly created `StorePath`
  -> NarSource MonadStore -- ^ provide nar stream
  -> Bool                 -- ^ Add target directory recursively
  -> RepairFlag           -- ^ Only used by local store backend
  -> MonadStore StorePath
addToStore name source recursive _repair = do
  runOpArgsIO AddToStore $ \yield -> do
    yield $ toStrict $ Data.Binary.Put.runPut $ (`runReaderT` ()) $ do
      put text $ System.Nix.StorePath.unStorePathName name
      put bool $ not $ System.Nix.Hash.algoName @a == "sha256" && recursive
      put bool recursive
      put text $ System.Nix.Hash.algoName @a
    source yield
  sockGetPath

-- | Add text to store.
--
-- Reference accepts repair but only uses it
-- to throw error in case of remote talking to nix-daemon.
addTextToStore
  :: Text         -- ^ Name of the text
  -> Text         -- ^ Actual text to add
  -> StorePathSet -- ^ Set of `StorePath`s that the added text references
  -> RepairFlag   -- ^ Repair flag, must be `False` in case of remote backend
  -> MonadStore StorePath
addTextToStore name bytes references' repair = do
  when repair
    $ error "repairing is not supported when building through the Nix daemon"
  runOpArgs AddTextToStore $ do
    put text name
    put text bytes
    put (hashSet path) references'
  sockGetPath

addSignatures :: StorePath -> [BSL.ByteString] -> MonadStore ()
addSignatures p signatures = do
  void $ simpleOpArgs AddSignatures $ do
    put path p
    put (list lazyByteStringLen) signatures

addIndirectRoot :: StorePath -> MonadStore ()
addIndirectRoot pn = do
  void $ simpleOpArgs AddIndirectRoot $ put path pn

-- | Add temporary garbage collector root.
--
-- This root is removed as soon as the client exits.
addTempRoot :: StorePath -> MonadStore ()
addTempRoot pn = do
  void $ simpleOpArgs AddTempRoot $ put path pn

-- | Build paths if they are an actual derivations.
--
-- If derivation output paths are already valid, do nothing.
buildPaths :: StorePathSet -> BuildMode -> MonadStore ()
buildPaths ps bm = do
  void $ simpleOpArgs BuildPaths $ do
    put (hashSet path) ps
    put int $ fromEnum bm

buildDerivation
  :: StorePath
  -> Derivation StorePath Text
  -> BuildMode
  -> MonadStore BuildResult
buildDerivation p drv buildMode = do
  runOpArgs BuildDerivation $ do
    put path p
    put derivation drv
    put enum buildMode
    -- XXX: reason for this is unknown
    -- but without it protocol just hangs waiting for
    -- more data. Needs investigation.
    -- Intentionally the only warning that should pop-up.
    put int (0 :: Integer)

  getSocketIncremental $ get buildResult

ensurePath :: StorePath -> MonadStore ()
ensurePath pn = do
  void $ simpleOpArgs EnsurePath $ put path pn

-- | Find garbage collector roots.
findRoots :: MonadStore (Map BSL.ByteString StorePath)
findRoots = do
  runOp FindRoots
  r <-
    getSocketIncremental
    $ get
    $ list
    $ tup lazyByteStringLen path

  pure $ Data.Map.Strict.fromList r
{-
 where
  catRights :: [(a, Either String b)] -> MonadStore [(a, b)]
  catRights = mapM ex

  ex :: (a, Either [Char] b) -> MonadStore (a, b)
  ex (x , Right y) = pure (x, y)
  ex (_x, Left e ) = error $ "Unable to decode root: " <> fromString e
-}

isValidPathUncached :: StorePath -> MonadStore Bool
isValidPathUncached p = do
  simpleOpArgs IsValidPath $ put path p

-- | Query valid paths from set, optionally try to use substitutes.
queryValidPaths
  :: StorePathSet   -- ^ Set of `StorePath`s to query
  -> SubstituteFlag -- ^ Try substituting missing paths when `True`
  -> MonadStore StorePathSet
queryValidPaths ps substitute = do
  runOpArgs QueryValidPaths $ do
    put (hashSet path) ps
    put bool substitute
  sockGetPaths

queryAllValidPaths :: MonadStore StorePathSet
queryAllValidPaths = do
  runOp QueryAllValidPaths
  sockGetPaths

querySubstitutablePaths :: StorePathSet -> MonadStore StorePathSet
querySubstitutablePaths ps = do
  runOpArgs QuerySubstitutablePaths $ put (hashSet path)  ps
  sockGetPaths

queryPathInfoUncached :: StorePath -> MonadStore (StorePath, StorePathMetadata)
queryPathInfoUncached key = do
  runOpArgs QueryPathInfo $ do
    put path key

  valid <- sockGet $ get bool
  unless valid $ error "Path is not valid"

  meta <- getSocketIncremental $ get pathMetadata

  pure $ (key, meta)

queryReferrers :: StorePath -> MonadStore StorePathSet
queryReferrers p = do
  runOpArgs QueryReferrers $ put path p
  sockGetPaths

queryValidDerivers :: StorePath -> MonadStore StorePathSet
queryValidDerivers p = do
  runOpArgs QueryValidDerivers $ put path p
  sockGetPaths

queryDerivationOutputs :: StorePath -> MonadStore StorePathSet
queryDerivationOutputs p = do
  runOpArgs QueryDerivationOutputs $ put path p
  sockGetPaths

queryDerivationOutputNames :: StorePath -> MonadStore StorePathSet
queryDerivationOutputNames p = do
  runOpArgs QueryDerivationOutputNames $ put path p
  sockGetPaths

queryPathFromHashPart :: StorePathHashPart -> MonadStore StorePath
queryPathFromHashPart storePathHash = do
  runOpArgs QueryPathFromHashPart
    $ put byteStringLen
    $ encodeUtf8 (encodeWith NixBase32 $ coerce storePathHash)
  sockGetPath

queryMissing
  :: StorePathSet
  -> MonadStore
      ( StorePathSet-- Paths that will be built
      , StorePathSet -- Paths that have substitutes
      , StorePathSet -- Unknown paths
      , Integer            -- Download size
      , Integer            -- Nar size?
      )
queryMissing ps = do
  runOpArgs QueryMissing $ put (hashSet path) ps

  willBuild      <- sockGetPaths
  willSubstitute <- sockGetPaths
  unknown        <- sockGetPaths
  downloadSize'  <- sockGet $ get int
  narSize'       <- sockGet $ get int
  pure (willBuild, willSubstitute, unknown, downloadSize', narSize')

optimiseStore :: MonadStore ()
optimiseStore = void $ simpleOp OptimiseStore

syncWithGC :: MonadStore ()
syncWithGC = void $ simpleOp SyncWithGC

-- returns True on errors
verifyStore :: CheckFlag -> RepairFlag -> MonadStore Bool
verifyStore check repair = simpleOpArgs VerifyStore $ do
  put bool check
  put bool repair

-- For convenience:

sockGetPath :: MonadStore StorePath
sockGetPath = sockGet $ get path

sockGetPaths :: MonadStore StorePathSet
sockGetPaths = do
  sockGet $ get $ hashSet path
