{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module System.Nix.Store.Remote.Server where

import           Prelude                       hiding ( bool, put, get )

import           Control.Concurrent.Classy.Async
import           Control.Monad.Conc.Class (MonadConc)
import           Control.Monad.Catch
import qualified Data.HashSet as HashSet
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)
import           Network.Socket
import           Network.Socket.ByteString

import           System.Nix.StorePath ( StoreDir
                                      , StorePath
                                      , StorePathName(..)
                                      )
import           System.Nix.ValidPathInfo

import           System.Nix.Nar                 ( NarSource )

import           System.Nix.Store.Remote.Binary as RB
import           System.Nix.Store.Remote.Socket
import           System.Nix.Store.Remote.GADT as R
import qualified System.Nix.Store.Remote.Protocol as P
import           System.Nix.Store.Remote.Protocol hiding ( protoVersion )
import           System.Nix.Store.Remote.MonadStore ( StoreConfig(..) )


type WorkerHelper m = forall a. StoreRequest a -> m a

-- | run an emulated nix daemon on given socket address.  the deamon will close
-- when the continuation returns.
runDaemonSocket
  :: forall m a
  . (MonadIO m, MonadConc m)
  => StoreDir
  -> WorkerHelper m
  -> Socket
  -> m a
  -> m a
runDaemonSocket sd workerHelper lsock k = do
  liftIO $ listen lsock maxListenQueue

  liftIO $ T.putStrLn "listening"

  let listener :: m Void
      listener = do
        (sock, _) <- liftIO $ accept lsock
        liftIO $ T.putStrLn "accepting"

        -- TODO: this, but without the space leak
        fmap fst $ concurrently listener $ processConnection sd workerHelper sock

  either absurd id <$> race listener k


-- | fatal error in worker interaction which should disconnect client.
data WorkerException
  = WorkerException_ClientVersionTooOld
  | WorkerException_ProtocolMismatch
  | WorkerException_Error WorkerError
  -- ^ allowed error outside allowed worker state
  | WorkerException_DecodingError DecodingError
  | WorkerException_BuildFailed StorePath
  deriving (Eq, Ord, Show)

-- | non-fatal (to server) errors in worker interaction
data WorkerError
  = WorkerError_SendClosed
  -- | WorkerError_ClientVersionTooOld
  | WorkerError_InvalidOperation Word64
  | WorkerError_UnsupportedOperation String
  | WorkerError_InvalidPath StorePath
  deriving (Eq, Ord, Show)

instance Exception WorkerException

-- | "main loop" of the daemon for a single connection.
--
-- this function should take care to not throw errors from client connections.
processConnection
  :: (MonadIO m, MonadMask m)
  => StoreDir
  -> WorkerHelper m
  -> Socket
  -> m ()
processConnection sd workerHelper sock = do
  ~() <- flip runReaderT sock $ do
    -- Exchange the greeting.
    magic <- sockGet $ get int
    liftIO $ print ("magic" :: Text, magic)
    when (magic /= workerMagic1) $ throwM WorkerException_ProtocolMismatch
    sockPut $ put int workerMagic2
    sockPut $ put protoVersion ourProtoVersion

    clientVersion@(ProtoVersion _ clientMinorVersion) <- sockGet $ get protoVersion
    liftIO $ print clientVersion

    when (clientVersion < ProtoVersion 0x1 0x0a) $
        throwM WorkerException_ClientVersionTooOld

    tunnelLogger <- liftIO $ newTunnelLogger

    when (clientMinorVersion >= 14) $ do
      x :: Word32 <- sockGet $ get int
      when (x /= 0) $ do
        -- Obsolete CPU affinity.
        _ :: Word32 <- sockGet $ get int
        pure ()

    when (clientMinorVersion >= 11) $ do
        _ :: Word32 <- sockGet $ get int -- obsolete reserveSpace
        pure ()

    when (clientMinorVersion >= 33) $ do
        sockPut $ put text "nixVersion (smithy)"

    -- Send startup error messages to the client.
    startWork tunnelLogger

    -- TODO: do we need auth at all?  probably?
    -- If we can't accept clientVersion, then throw an error *here* (not above).
    --authHook(*store);

    stopWork tunnelLogger

    -- Process client requests.
    let loop = do
          op <- sockGet (get enum)
          lift $ performOp sd workerHelper sock clientVersion tunnelLogger op
          loop
    loop

  liftIO $ T.putStrLn "daemon connection done"
  liftIO $ close sock

simpleOp
  :: ( MonadIO m
     , HasStoreSocket r
     , MonadReader r m
     )
  => (StoreRequest () -> m ())
  -> TunnelLogger r
  -> m (StoreRequest ())
  -> m ()
simpleOp workerHelper tunnelLogger m = do
  req <- m
  bracketLogger tunnelLogger $ workerHelper req
  sockPut $ put bool True

simpleOpRet
  :: ( MonadIO m
     , HasStoreSocket r
     , MonadReader r m
     )
  => (StoreRequest a -> m a)
  -> TunnelLogger r
  -> Serializer r a
  -> m (StoreRequest a)
  -> m ()
simpleOpRet workerHelper tunnelLogger s m = do
  req <- m
  resp <- bracketLogger tunnelLogger $ workerHelper req
  sockPut $ put s resp

bracketLogger
  :: ( MonadIO m
     , HasStoreSocket r
     , MonadReader r m
     )
  => TunnelLogger r
  -> m a
  -> m a
bracketLogger tunnelLogger m = do
  startWork tunnelLogger
  a <- m
  stopWork tunnelLogger
  pure a

{-# WARNING unimplemented "not yet implemented" #-}
unimplemented :: WorkerException
unimplemented = WorkerException_Error $ WorkerError_UnsupportedOperation "not yet implemented"

performOp
  :: forall m
  .  ( MonadIO m
     , MonadThrow m
     )
  => StoreDir
  -> WorkerHelper m
  -> Socket
  -> ProtoVersion
  -> TunnelLogger Socket
  -> WorkerOp
  -> m ()
performOp sd workerHelper sock protocolVersion tunnelLogger0 op = do
  tunnelLogger <- liftIO $ mapTunnelLogger storeConfig_socket tunnelLogger0
  let simpleOp' = simpleOp (lift . workerHelper) tunnelLogger
  let simpleOpRet'
        :: Serializer StoreConfig a
        -> ReaderT StoreConfig m (StoreRequest a)
        -> ReaderT StoreConfig m ()
      simpleOpRet' = simpleOpRet (lift . workerHelper) tunnelLogger
  let invalidOp = WorkerException_Error $ WorkerError_InvalidOperation $ fromIntegral $ fromEnum op
  let

  flip runReaderT (StoreConfig sd protocolVersion sock) $ case op of
    Reserved_0__ -> throwM invalidOp

    P.IsValidPath -> simpleOpRet' bool $
      sockGet $ R.IsValidPath
        <$> get path

    Reserved_2__ -> throwM invalidOp

    HasSubstitutes -> do
      p <- sockGet $ get path
      s <- bracketLogger tunnelLogger $
        lift . workerHelper $ R.QuerySubstitutablePaths $ HashSet.singleton p
      sockPut $ put bool $ not $ HashSet.null s

    QueryPathHash -> throwM unimplemented
    QueryReferences -> throwM unimplemented

    P.QueryReferrers -> simpleOpRet' (hashSet path) $
      sockGet $ R.QueryReferrers
        <$> get path

    P.AddToStore -> do
      req <- sockGet $ do
        name <- StorePathName <$> get text
        _ignore <- get bool
        recursive <- get bool
        algo <- get someHashAlgo
        socket <- asks storeSocket
        let source :: forall m2. MonadIO m2 => NarSource m2
            source k = k =<< liftIO (recv sock 8)
        pure $ R.AddToStore name recursive algo source False
      p <- bracketLogger tunnelLogger $ lift $ workerHelper req
      sockPut $ put path p

    P.AddTextToStore -> do
      req <- sockGet $ R.AddTextToStore
        <$> get text
        <*> get text
        <*> get (hashSet path)
        <*> pure False -- repairing is not supported when building through the Nix daemon
      p <- bracketLogger tunnelLogger $ lift $ workerHelper req
      sockPut $ put path p

    P.BuildPaths -> simpleOp' $sockGet $
      R.BuildPaths
        <$> get (hashSet path)
        <*> get enum

    P.EnsurePath -> simpleOp' $ sockGet $
      R.EnsurePath
        <$> get path

    P.AddTempRoot -> simpleOp' $ sockGet $
      R.AddTempRoot
        <$> get path

    P.AddIndirectRoot -> simpleOp' $ sockGet $
      R.AddIndirectRoot
        <$> get path

    P.SyncWithGC -> simpleOp' $ pure R.SyncWithGC

    P.FindRoots -> simpleOpRet'
      (strictMap lazyByteStringLen path)
      $ pure R.FindRoots

    Reserved_15__ -> throwM invalidOp
    P.ExportPath -> throwM unimplemented
    Reserved_17__ -> throwM invalidOp

    QueryDeriver -> do
      p <- sockGet $ get path
      m_info <- bracketLogger tunnelLogger $
        lift . workerHelper $ R.QueryPathInfo p
      sockPut $ put maybePath $ deriverPath =<< m_info

    SetOptions -> throwM unimplemented
    CollectGarbage -> throwM unimplemented
    QuerySubstitutablePathInfo -> throwM unimplemented

    P.QueryDerivationOutputs -> simpleOpRet' (hashSet path) $ sockGet $
      R.QueryDerivationOutputs
        <$> get path

    P.QueryAllValidPaths -> simpleOpRet' (hashSet path) $
      pure $ R.QueryAllValidPaths

    P.QueryFailedPaths -> throwM unimplemented
    P.ClearFailedPaths -> throwM unimplemented

    P.QueryPathInfo -> simpleOpRet' (RB.maybe pathMetadata) $
      sockGet $ R.QueryPathInfo
        <$> get path

    P.ImportPaths -> throwM unimplemented

    P.QueryDerivationOutputNames -> simpleOpRet' (hashSet storePathName) $ sockGet $
      R.QueryDerivationOutputNames
        <$> get path

    P.QueryPathFromHashPart -> simpleOpRet' path $ sockGet $
      R.QueryPathFromHashPart
        <$> get storePathHashPart

    QuerySubstitutablePathInfos -> throwM unimplemented

    P.QueryValidPaths -> simpleOpRet' (hashSet path) $
      sockGet $ R.QueryValidPaths
        <$> get (hashSet path)
        <*> get bool

    P.QuerySubstitutablePaths -> simpleOpRet' (hashSet path) $
      sockGet $ R.QuerySubstitutablePaths
        <$> get (hashSet path)

    P.QueryValidDerivers -> simpleOpRet' (hashSet path) $
      sockGet $ R.QueryValidDerivers
        <$> get path

    P.OptimiseStore -> simpleOp' $ pure R.OptimiseStore

    P.VerifyStore -> simpleOpRet' bool $ sockGet $
      R.VerifyStore
        <$> get bool
        <*> get bool

    P.BuildDerivation -> simpleOpRet' buildResult $ sockGet $
      R.BuildDerivation
        <$> get path
        <*> get derivation
        <*> get enum
      <* get (int :: Serializer r Word32)

    P.AddSignatures -> simpleOp' $ sockGet $
      R.AddSignatures
        <$> get path
        <*> get (list lazyByteStringLen)

---

data TunnelLogger r = TunnelLogger
  { _tunnelLogger_state :: IORef (TunnelLoggerState r)
  }

data TunnelLoggerState r = TunnelLoggerState
  { _tunnelLoggerState_canSendStderr :: Bool
  , _tunnelLoggerState_pendingMsgs :: [Put r]
  }

newTunnelLogger :: IO (TunnelLogger r)
newTunnelLogger = TunnelLogger <$> newIORef (TunnelLoggerState False [])

mapTunnelLogger
  :: (s -> r)
  -> TunnelLogger r
  -> IO (TunnelLogger s)
mapTunnelLogger f logger = do
  s <- readIORef (_tunnelLogger_state logger)
  stateRef <- newIORef $ TunnelLoggerState
    { _tunnelLoggerState_canSendStderr = _tunnelLoggerState_canSendStderr s
    , _tunnelLoggerState_pendingMsgs = withReaderT f <$> _tunnelLoggerState_pendingMsgs s
    }
  pure $ TunnelLogger
    { _tunnelLogger_state = stateRef
    }
enqueueMsg
  :: (MonadIO m, MonadReader r m, HasStoreSocket r)
  => TunnelLogger r
  -> Put r
  -> m ()
enqueueMsg x s = updateLogger x $ \st@(TunnelLoggerState c p) -> case c of
  True -> (st, sockPut s)
  False -> (TunnelLoggerState c (s:p), pure ())

log
  :: (MonadIO m, MonadReader r m, HasStoreSocket r)
  => TunnelLogger r
  -> LBS.ByteString
  -> m ()
log l s = enqueueMsg l (put int StderrNext >> put lazyByteStringLen s)

startWork
  :: (MonadIO m, MonadReader r m, HasStoreSocket r)
  => TunnelLogger r
  -> m ()
startWork x = updateLogger x $ \(TunnelLoggerState _ p) -> (,)
  (TunnelLoggerState True []) $
  (traverse_ sockPut $ reverse p)

stopWork
  :: (MonadIO m, MonadReader r m, HasStoreSocket r)
  => TunnelLogger r
  -> m ()
stopWork x = updateLogger x $ \_ -> (,)
  (TunnelLoggerState False [])
  (sockPut $ put int StderrLast)

-- | Stop sending logging and report an error.
--
-- Returns true if the the session was in a state that allowed the error to be
-- sent.
--
-- Unlike 'stopWork', this function may be called at any time to (try) to end a
-- session with an error.
stopWorkOnError
  :: (MonadIO m, MonadReader r m, HasStoreSocket r, HasProtoVersion r)
  => TunnelLogger r
  -> ErrorInfo
  -> m Bool
stopWorkOnError x ex = updateLogger x $ \st ->
  case _tunnelLoggerState_canSendStderr st of
    False -> (st, pure False)
    True -> (,) (TunnelLoggerState False []) $ do
      asks P.protoVersion >>= \pv -> if protoVersion_minor pv >= 26
        then sockPut $ put int StderrError >> put errorInfo ex
        else sockPut $ put int StderrError >> put text (T.pack $ show ex) >> put int (0 :: Word)
      pure True

updateLogger
  :: (MonadIO m, MonadReader r m, HasStoreSocket r)
  => TunnelLogger r
  -> (TunnelLoggerState r -> (TunnelLoggerState r, m a))
  -> m a
updateLogger x = join . liftIO . atomicModifyIORef (_tunnelLogger_state x)
