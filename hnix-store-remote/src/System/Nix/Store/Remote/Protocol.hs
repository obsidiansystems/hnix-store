{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module System.Nix.Store.Remote.Protocol
  ( WorkerOp(..)
  , simpleOp
  , simpleOpArgs
  , runOp
  , runOpArgs
  , runOpArgsIO
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
import           System.Nix.Store.Remote.Types hiding ( protoVersion )
import           System.Nix.Store.Remote.Socket


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

-- | worker opcode
--
-- This type has gaps filled in so that the GHC builtin Enum instance lands on the right values.
data WorkerOp
  = Reserved_0__ -- 0
  | IsValidPath -- 1
  | Reserved_2__ -- 2
  | HasSubstitutes -- 3
  | QueryPathHash -- 4 // obsolete
  | QueryReferences --  5 // obsolete
  | QueryReferrers --  6
  | AddToStore --  7
  | AddTextToStore --  8 // obsolete since 1.25, Nix 3.0. Use wopAddToStore
  | BuildPaths --  9
  | EnsurePath --  10 0xa
  | AddTempRoot --  11 0xb
  | AddIndirectRoot --  12 0xc
  | SyncWithGC --  13 0xd
  | FindRoots --  14 0xe
  | Reserved_15__ -- 15 0xf
  | ExportPath --  16 0x10 // obsolete
  | Reserved_17__ -- 17 0x11
  | QueryDeriver --  18 0x12 // obsolete
  | SetOptions --  19 0x13
  | CollectGarbage --  20 0x14
  | QuerySubstitutablePathInfo --  21 0x15
  | QueryDerivationOutputs --  22 0x16 // obsolete
  | QueryAllValidPaths --  23 0x17
  | QueryFailedPaths --  24 0x18
  | ClearFailedPaths --  25 0x19
  | QueryPathInfo --  26 0x1a
  | ImportPaths --  27 0x1b // obsolete
  | QueryDerivationOutputNames --  28 0x1c // obsolete
  | QueryPathFromHashPart --  29 0x1d
  | QuerySubstitutablePathInfos --  30 0x1e
  | QueryValidPaths --  31 0x1f
  | QuerySubstitutablePaths --  32 0x20
  | QueryValidDerivers --  33 0x21
  | OptimiseStore --  34 0x22
  | VerifyStore --  35 0x23
  | BuildDerivation --  36 0x24
  | AddSignatures --  37 0x25
  | NarFromPath --  38 0x26
  | AddToStoreNar --  39 0x27
  | QueryMissing --  40 0x28
  | QueryDerivationOutputMap --  41 0x29
  | RegisterDrvOutput --  42 0x2a
  | QueryRealisation --  43 0x2b
  | AddMultipleToStore --  44 0x2c
  | AddBuildLog --  45 0x2d
  | BuildPathsWithResults --  46 0x2e
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

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

runStore :: MonadStore a -> IO (Either String a, [Logger])
runStore = runStoreOpts defaultSockPath $ StoreDir "/nix/store"

runStoreOpts
  :: FilePath -> StoreDir -> MonadStore a -> IO (Either String a, [Logger])
runStoreOpts socketPath = runStoreOpts' S.AF_UNIX (SockAddrUnix socketPath)

runStoreOptsTCP
  :: String -> Int -> StoreDir -> MonadStore a -> IO (Either String a, [Logger])
runStoreOptsTCP host port storeRootDir code = do
  S.getAddrInfo (Just S.defaultHints) (Just host) (Just $ show port) >>= \case
    (sockAddr:_) -> runStoreOpts' (S.addrFamily sockAddr) (S.addrAddress sockAddr) storeRootDir code
    _ -> pure (Left "Couldn't resolve host and port with getAddrInfo.", [])

runStoreOpts'
  :: S.Family -> S.SockAddr -> StoreDir -> MonadStore a -> IO (Either String a, [Logger])
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

runMonadStore0 :: r -> MonadStore0 r a -> IO (Either String a, [Logger])
runMonadStore0 r = fmap (\(res, (_data, logs)) -> (res, logs))
  . (`runReaderT` r)
  . (`runStateT` (Nothing, []))
  . runExceptT
