{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module System.Nix.Store.Remote.Client
  ( simpleOp
  , simpleOpArgs
  , runOp
  , runOpArgs
  , runOpArgsIO
  , Run
  , runStore
  , runStoreOpts
  , runStoreOptsTCP
  , runStoreOpts'
  )
where

import           Prelude                       hiding ( bool, put, get )

import           Control.Exception              ( bracket )

import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString

import           Network.Socket                 ( SockAddr(SockAddrUnix) )
import qualified Network.Socket                 as S
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )

import           System.Nix.StorePath           ( StoreDir(..) )
import           System.Nix.Store.Remote.Binary
import           System.Nix.Store.Remote.Logger
import           System.Nix.Store.Remote.MonadStore
import           System.Nix.Store.Remote.Socket
import           System.Nix.Store.Remote.Protocol hiding ( protoVersion )


ourProtoVersion :: ProtoVersion
ourProtoVersion = ProtoVersion
  { protoVersion_major = 1
  , protoVersion_minor = 21
  }

workerMagic1 :: Int32
workerMagic1 = 0x6e697863
workerMagic2 :: Int32
workerMagic2 = 0x6478696f

defaultSockPath :: String
defaultSockPath = "/nix/var/nix/daemon-socket/socket"

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
  bracket open (S.close . storeSocket) run

 where
  open = do
    soc <- S.socket sockFamily S.Stream 0
    S.connect soc sockAddr
    pure PreStoreConfig
        { preStoreConfig_socket = soc
        , preStoreConfig_dir = storeRootDir
        }

  greet :: MonadStore0 PreStoreConfig ProtoVersion
  greet = do
    sockPut $ put int workerMagic1
    soc      <- asks storeSocket
    vermagic <- liftIO $ recv soc 16
    let
      (magic2, daemonProtoVersion) =
        flip B.runGet (fromStrict vermagic)
          $ (`runReaderT` ())
          $ (,)
            <$> (get int :: Get () Int32)
            <*> get protoVersion
    unless (magic2 == workerMagic2) $ error "Worker magic 2 mismatch"

    sockPut $ put protoVersion ourProtoVersion -- clientVersion
    sockPut $ put int (0 :: Int)   -- affinity
    sockPut $ put int (0 :: Int)   -- obsolete reserveSpace

    processOutput_
    -- TODO should be min
    pure daemonProtoVersion

  run preStoreConfig = runMonadStore0 preStoreConfig $ do
    pv <- greet
    mapConnectionInfo
      (\(PreStoreConfig a b) -> StoreConfig a pv b)
      code

type Run a = IO (Either String a, [Logger])

runMonadStore0 :: r -> MonadStore0 r a -> Run a
runMonadStore0 r = fmap (\(res, (_data, logs)) -> (res, logs))
  . (`runReaderT` r)
  . (`runStateT` (Nothing, []))
  . runExceptT
