{-# language AllowAmbiguousTypes #-}
{-# language KindSignatures      #-}
{-# language GADTs #-}
{-# language RankNTypes          #-}
{-# language ScopedTypeVariables #-}
{-# language DataKinds           #-}
{-# language RecordWildCards     #-}
{-# language LiberalTypeSynonyms #-}

module System.Nix.Store.Remote
  ( doReq
  , module System.Nix.Store.Remote.Types
  )
where

import           Prelude                 hiding ( bool, put, get )

import           System.Nix.Hash                ( NamedAlgo(..)
                                                , BaseEncoding(NixBase32)
                                                )
import           System.Nix.StorePath           ( StorePath
                                                , StorePathSet
                                                )
import           System.Nix.Internal.Base       ( encodeWith )

import qualified Data.Binary.Put
import qualified Data.Map.Strict

import qualified System.Nix.StorePath

import           System.Nix.Store.Remote.Binary
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol hiding (WorkerOp(..))
import qualified System.Nix.Store.Remote.Protocol as P
import           System.Nix.Store.Remote.Socket
import           System.Nix.Store.Remote.GADT as R
import           System.Nix.Nar                 ( NarSource )

doReq :: StoreRequest a -> MonadStore a
doReq = \case
  R.AddToStore (Proxy :: Proxy a) name source recursive _repair -> do
    runOpArgsIO P.AddToStore $ \yield -> do
      yield $ toStrict $ Data.Binary.Put.runPut $ (`runReaderT` ()) $ do
        put text $ System.Nix.StorePath.unStorePathName name
        put bool $ not $ System.Nix.Hash.algoName @a == "sha256" && recursive
        put bool recursive
        put text $ System.Nix.Hash.algoName @a
      source yield
    sockGetPath

  R.AddTextToStore name bytes references' repair -> do
    when repair
      $ error "repairing is not supported when building through the Nix daemon"
    runOpArgs P.AddTextToStore $ do
      put text name
      put text bytes
      put (hashSet path) references'
    sockGetPath

  R.AddSignatures p signatures -> do
    void $ simpleOpArgs P.AddSignatures $ do
      put path p
      put (list lazyByteStringLen) signatures

  R.AddIndirectRoot pn -> do
    void $ simpleOpArgs P.AddIndirectRoot $ put path pn

  R.AddTempRoot pn -> do
    void $ simpleOpArgs P.AddTempRoot $ put path pn

  R.BuildPaths ps bm -> do
    void $ simpleOpArgs P.BuildPaths $ do
      put (hashSet path) ps
      put int $ fromEnum bm

  R.BuildDerivation p drv buildMode -> do
    runOpArgs P.BuildDerivation $ do
      put path p
      put derivation drv
      put enum buildMode
      -- XXX: reason for this is unknown
      -- but without it protocol just hangs waiting for
      -- more data. Needs investigation.
      -- Intentionally the only warning that should pop-up.
      put int (0 :: Integer)
    getSocketIncremental $ get buildResult

  R.EnsurePath pn -> do
    void $ simpleOpArgs P.EnsurePath $ put path pn

  R.FindRoots -> do
    runOp P.FindRoots
    r <- getSocketIncremental $ get $ list $ tup lazyByteStringLen path
    pure $ Data.Map.Strict.fromList r
  {-
   where
    catRights :: [(a, Either String b)] -> MonadStore [(a, b)]
    catRights = mapM ex

    ex :: (a, Either [Char] b) -> MonadStore (a, b)
    ex (x , Right y) = pure (x, y)
    ex (_x, Left e ) = error $ "Unable to decode root: " <> fromString e
  -}

  R.IsValidPathUncached p -> do
    simpleOpArgs P.IsValidPath $ put path p

  R.QueryValidPaths ps substitute -> do
    runOpArgs P.QueryValidPaths $ do
      put (hashSet path) ps
      put bool substitute
    sockGetPaths

  R.QueryAllValidPaths -> do
    runOp P.QueryAllValidPaths
    sockGetPaths

  R.QuerySubstitutablePaths ps -> do
    runOpArgs P.QuerySubstitutablePaths $ put (hashSet path) ps
    sockGetPaths

  R.QueryPathInfoUncached key -> do
    runOpArgs P.QueryPathInfo $ do
      put path key
    valid <- sockGet $ get bool
    unless valid $ error "Path is not valid"
    meta <- getSocketIncremental $ get pathMetadata
    pure $ (key, meta)

  R.QueryReferrers p -> do
    runOpArgs P.QueryReferrers $ put path p
    sockGetPaths

  R.QueryValidDerivers p -> do
    runOpArgs P.QueryValidDerivers $ put path p
    sockGetPaths

  R.QueryDerivationOutputs p -> do
    runOpArgs P.QueryDerivationOutputs $ put path p
    sockGetPaths

  R.QueryDerivationOutputNames p -> do
    runOpArgs P.QueryDerivationOutputNames $ put path p
    sockGetPaths

  R.QueryPathFromHashPart storePathHash -> do
    runOpArgs P.QueryPathFromHashPart
      $ put byteStringLen
      $ encodeUtf8 (encodeWith NixBase32 $ coerce storePathHash)
    sockGetPath

  R.QueryMissing ps -> do
    runOpArgs P.QueryMissing $ put (hashSet path) ps

    willBuild      <- sockGetPaths
    willSubstitute <- sockGetPaths
    unknown        <- sockGetPaths
    downloadSize'  <- sockGet $ get int
    narSize'       <- sockGet $ get int
    pure (willBuild, willSubstitute, unknown, downloadSize', narSize')

  R.OptimiseStore -> do
    void $ simpleOp P.OptimiseStore

  R.SyncWithGC -> do
    void $ simpleOp P.SyncWithGC

  R.VerifyStore check repair -> do
    simpleOpArgs P.VerifyStore $ do
      put bool check
      put bool repair

-- For convenience:

sockGetPath :: MonadStore StorePath
sockGetPath = sockGet $ get path

sockGetPaths :: MonadStore StorePathSet
sockGetPaths = do
  sockGet $ get $ hashSet path
