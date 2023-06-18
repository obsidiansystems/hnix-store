{-# language AllowAmbiguousTypes #-}
{-# language KindSignatures      #-}
{-# language RankNTypes          #-}
{-# language ScopedTypeVariables #-}
{-# language DataKinds           #-}
{-# language RecordWildCards     #-}
{-# language LiberalTypeSynonyms #-}

module System.Nix.Store.Remote
  ( runStore
  , runStoreOpts
  , runStoreOptsTCP
  , runDaemon
  , runDaemonOpts
  , module System.Nix.Store.Remote.MonadStore
  )
where

import           Prelude                 hiding ( IORef, bool, put, get )

import           Control.Exception.Lifted       ( bracket )
import           Control.Monad.Trans.Control    hiding ( Run )
import           Control.Monad.Conc.Class

import           Network.Socket                 ( SockAddr(SockAddrUnix) )
import qualified Network.Socket                 as S

import           System.Directory

import           System.Nix.StorePath           ( StoreDir (..) )

import           System.Nix.Store.Remote.MonadStore
import           System.Nix.Store.Remote.Socket

import           System.Nix.Store.Remote.Client
import           System.Nix.Store.Remote.Server

defaultSockPath :: String
defaultSockPath = "/nix/var/nix/daemon-socket/socket"

runStore :: MonadStore a -> Run a
runStore = runStoreOpts defaultSockPath $ StoreDir "/nix/store"

runStoreOpts
  :: FilePath -> StoreDir -> MonadStore a -> Run a
runStoreOpts socketPath = runStoreOpts' S.AF_UNIX (SockAddrUnix socketPath)

runStoreOptsTCP
  :: String -> Int -> StoreDir -> MonadStore a -> Run a
runStoreOptsTCP host port storeRootDir code = do
  S.getAddrInfo (Just S.defaultHints) (Just host) (Just $ show port) >>= \case
    (sockAddr:_) -> runStoreOpts' (S.addrFamily sockAddr) (S.addrAddress sockAddr) storeRootDir code
    _ -> pure (Left "Couldn't resolve host and port with getAddrInfo.", [])

runStoreOpts'
  :: S.Family -> S.SockAddr -> StoreDir -> MonadStore a -> Run a
runStoreOpts' sockFamily sockAddr storeRootDir code =
  bracket open (S.close . storeSocket) (flip runStoreSocket code)

 where
  open = do
    soc <- S.socket sockFamily S.Stream 0
    S.connect soc sockAddr
    pure PreStoreConfig
        { preStoreConfig_socket = soc
        , preStoreConfig_dir = storeRootDir
        }

runDaemon
  :: forall m a
  . ( MonadIO m
    , MonadBaseControl IO m
    , MonadConc m
    )
  => WorkerHelper m
  -> m a
  -> m a
runDaemon workerHelper k = runDaemonOpts (StoreDir "/nix/store") workerHelper defaultSockPath k

-- | run an emulated nix daemon on given socket address.  the deamon will close
-- when the continuation returns.
runDaemonOpts
  :: forall m a
  . ( MonadIO m
    , MonadBaseControl IO m
    , MonadConc m
    )
  => StoreDir
  -> WorkerHelper m
  -> FilePath
  -> m a
  -> m a
runDaemonOpts sd workerHelper f k = bracket
  (liftIO $ S.socket S.AF_UNIX S.Stream S.defaultProtocol)
  (\lsock -> liftIO $ S.close lsock *> removeFile f)
  $ \lsock -> do
  --                                                                                                           ^^^^^^^^^^^^
  -- TODO:  this: ---------------------------------------------------------------------------------------------////////////
  -- should really be
  -- a file lock followed by unlink *before* bind rather than after close.  If
  -- the program crashes (or loses power or something) then a stale unix
  -- socket will stick around and prevent the daemon from starting.  using a
  -- lock file instead means only one "copy" of the daemon can hold the lock,
  -- and can safely unlink the socket before binding no matter how shutdown
  -- occured.

  -- set up the listening socket
  liftIO $ S.bind lsock (SockAddrUnix f)
  runDaemonSocket sd workerHelper lsock k
