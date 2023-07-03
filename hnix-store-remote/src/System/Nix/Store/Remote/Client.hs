{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module System.Nix.Store.Remote.Client
  ( simpleOp
  , simpleOpArgs
  , runOp
  , runOpArgs
  , runOpArgsIO
  , doReq
  , Run
  , runStoreSocket
  )
where

import           Prelude                       hiding ( bool, put, get )

import Control.Monad.Catch

import qualified Data.Binary.Put as B
import qualified Data.ByteString
import           Data.Some

import           Network.Socket.ByteString      ( sendAll
                                                )

import           System.Nix.Hash                ( HashAlgo(..)
                                                )
import           System.Nix.StorePath           ( StorePath
                                                , StorePathSet
                                                , StorePathName(..)
                                                )
import           System.Nix.Store.Remote.Binary as RB
import           System.Nix.Store.Remote.Logger
import           System.Nix.Store.Remote.MonadStore
import           System.Nix.Store.Remote.Socket
import           System.Nix.Store.Remote.Server (WorkerException(..))
import           System.Nix.Store.Remote.GADT as R
import qualified System.Nix.Store.Remote.Protocol as P
import           System.Nix.Store.Remote.Protocol hiding ( protoVersion )


simpleOp :: WorkerOp -> MonadStore Bool
simpleOp op = simpleOpArgs op pass

simpleOpArgs :: WorkerOp -> Put StoreConfig -> MonadStore Bool
simpleOpArgs op args = do
  runOpArgs op args
  sockGet $ get bool

runOp :: WorkerOp -> MonadStore ()
runOp op = runOpArgs op pass

runOpArgs :: WorkerOp -> Put StoreConfig -> MonadStore ()
runOpArgs op args = do
  r <- ask
  runOpArgsIO
    op
    (\encode -> encode $ toStrict $ B.runPut $ runReaderT args r)

runOpArgsIO
  :: WorkerOp
  -> ((Data.ByteString.ByteString -> MonadStore ()) -> MonadStore ())
  -> MonadStore ()
runOpArgsIO op encoder = do

  sockPut $ put enum op

  soc <- asks storeSocket
  encoder (liftIO . sendAll soc)

  processOutput_

doReq :: StoreRequest a -> MonadStore a
doReq = \case
  R.AddToStore name recursive sha@(Some hashAlgo) source _repair -> do
    runOpArgsIO P.AddToStore $ \yield -> do
      yield $ toStrict $ B.runPut $ (`runReaderT` ()) $ do
        put text $ System.Nix.StorePath.unStorePathName name
        put bool $ not $ recursive &&
          case hashAlgo of HashAlgo_SHA256 -> True; _ -> False
        put bool recursive
        put someHashAlgo sha
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
      put enum bm

  R.BuildDerivation p drv buildMode -> do
    runOpArgs P.BuildDerivation $ do
      put path p
      put derivation drv
      put enum buildMode
      -- XXX: reason for this is unknown
      -- but without it protocol just hangs waiting for
      -- more data. Needs investigation.
      -- Intentionally the only warning that should pop-up.
      put int (0 :: Word32)
    sockGet $ get buildResult

  R.EnsurePath pn -> do
    void $ simpleOpArgs P.EnsurePath $ put path pn

  R.FindRoots -> do
    runOp P.FindRoots
    sockGet $ get (strictMap lazyByteStringLen path)
  {-
   where
    catRights :: [(a, Either String b)] -> MonadStore [(a, b)]
    catRights = mapM ex

    ex :: (a, Either [Char] b) -> MonadStore (a, b)
    ex (x , Right y) = pure (x, y)
    ex (_x, Left e ) = error $ "Unable to decode root: " <> fromString e
  -}

  R.IsValidPath p -> do
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

  R.QueryPathInfo key -> do
    runOpArgs P.QueryPathInfo $ do
      put path key
    sockGet $ get (RB.maybe pathMetadata)

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
    sockGet $ get (hashSet storePathName)

  R.QueryPathFromHashPart storePathHash -> do
    runOpArgs P.QueryPathFromHashPart
      $ put storePathHashPart storePathHash
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

-- entry point

runStoreSocket :: PreStoreConfig -> MonadStore a -> Run a
runStoreSocket preStoreConfig code = runMonadStore0 preStoreConfig $ do
  pv <- greet
  mapConnectionInfo
    (\(PreStoreConfig a b) -> StoreConfig a pv b)
    code
 where
  greet :: MonadStore0 PreStoreConfig ProtoVersion
  greet = do
    sockPut $ put int workerMagic1
    soc      <- asks storeSocket

    magic <- sockGetS int
    daemonVersion <- sockGetS protoVersion

    unless (magic == workerMagic2) $ error "Worker magic 2 mismatch"

    when (daemonVersion < ProtoVersion 1 10) $
      throwM WorkerException_ClientVersionTooOld

    sockPutS protoVersion ourProtoVersion -- clientVersion

    when (daemonVersion >= ProtoVersion 1 14) $
      sockPutS int (0 :: Int)   -- affinity, obsolete

    when (daemonVersion >= ProtoVersion 1 11) $
      sockPutS bool False   -- obsolete reserveSpace

    when (daemonVersion >= ProtoVersion 1 33) $ do
      -- If we were buffering I/O, we would flush the output here.
      _daemonNixVersion <- sockGetS text
      return ()

    remoteTrustsUs <- if daemonVersion >= ProtoVersion 1 35
      then do
        sockGetS trustedFlag
      else do
        return Nothing

    processOutput_
    -- TODO should be min
    pure daemonVersion

type Run a = IO (Either String a, [Logger])

runMonadStore0 :: r -> MonadStore0 r a -> Run a
runMonadStore0 r = fmap (\(res, (_data, logs)) -> (res, logs))
  . (`runReaderT` r)
  . (`runStateT` (Nothing, []))
  . runExceptT
