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
  , runStoreOpts'
  , module System.Nix.Store.Remote.MonadStore
  )
where

import           Prelude                 hiding ( bool, put, get )

import           Control.Exception              ( bracket )

import           Network.Socket                 ( SockAddr(SockAddrUnix) )
import qualified Network.Socket                 as S

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
