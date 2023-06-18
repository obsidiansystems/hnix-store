{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module System.Nix.Store.Remote.Server where

import           Prelude                       hiding ( IORef, newIORef, atomicModifyIORef, bool, put, get )

import           Control.Concurrent.Classy.Async
import           Control.Monad.Conc.Class
import           Control.Monad.Catch
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)
import           Network.Socket

import           System.Nix.StorePath ( StoreDir
                                      , StorePath
                                      )

import           System.Nix.Store.Remote.Binary
import           System.Nix.Store.Remote.Socket
import           System.Nix.Store.Remote.GADT as R
import qualified System.Nix.Store.Remote.Protocol as P
import           System.Nix.Store.Remote.Protocol hiding ( protoVersion )


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
  -> (forall a. StoreRequest a -> m a)
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

performOp
  :: StoreDir
  -> WorkerHelper m
  -> Socket
  -> ProtoVersion
  -> TunnelLogger r
  -> WorkerOp
  -> m ()
performOp _sd _workerHelper _sock _protoVersion _tunnelLogger op = error $ "GOT TO " <> show op

---

data TunnelLogger r = TunnelLogger
  { _tunnelLogger_state :: IORef IO (TunnelLoggerState r)
  }

data TunnelLoggerState r = TunnelLoggerState
  { _tunnelLoggerState_canSendStderr :: Bool
  , _tunnelLoggerState_pendingMsgs :: [Put r]
  }

newTunnelLogger :: IO (TunnelLogger r)
newTunnelLogger = TunnelLogger <$> newIORef (TunnelLoggerState False [])

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
