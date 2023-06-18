{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
module System.Nix.Store.Remote.MonadStore
  ( StoreConfig(..)
  , PreStoreConfig(..)
  , MonadStore0
  , MonadStore
  , MonadStoreHandshake
  -- *
  , mapConnectionInfo
  , getStoreDir
  , getLog
  , flushLog
  , gotError
  , getError
  , setData
  , clearData
  )
where

import           Control.Monad.Trans.State.Strict (mapStateT)
import           Control.Monad.Trans.Except (mapExceptT)
import qualified Data.ByteString.Lazy          as BSL
import           Network.Socket                 ( Socket )

import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Socket
import           System.Nix.StorePath          ( StoreDir
                                               , HasStoreDir(..)
                                               , getStoreDir
                                               )

data PreStoreConfig = PreStoreConfig
  { preStoreConfig_dir    :: StoreDir
  , preStoreConfig_socket :: Socket
  }

instance HasStoreDir PreStoreConfig where
  storeDir = preStoreConfig_dir

instance HasStoreSocket PreStoreConfig where
  storeSocket = preStoreConfig_socket

data StoreConfig = StoreConfig
  { storeConfig_dir         :: StoreDir
  , storeConfig_protoVersion :: ProtoVersion
  , storeConfig_socket      :: Socket
  }

instance HasStoreDir StoreConfig where
  storeDir = storeConfig_dir

instance HasProtoVersion StoreConfig where
  protoVersion = storeConfig_protoVersion

instance HasStoreSocket StoreConfig where
  storeSocket = storeConfig_socket

type MonadStore0 r
  = ExceptT
      String
      (StateT (Maybe BSL.ByteString, [Logger]) (ReaderT r IO))

type MonadStoreHandshake = MonadStore0 PreStoreConfig

type MonadStore = MonadStore0 StoreConfig

mapConnectionInfo :: (rb -> ra) -> (MonadStore0 ra c -> MonadStore0 rb c)
mapConnectionInfo = mapExceptT .  mapStateT . withReaderT

viewError :: Logger -> Maybe (Word, ByteString)
viewError (Error x y) = Just (x, y)
viewError _           = Nothing

isError :: Logger -> Bool
isError = isJust . viewError

gotError :: MonadStore0 r Bool
gotError = gets (any isError . snd)

getError :: MonadStore0 r [(Word, ByteString)]
getError = gets (mapMaybe viewError . snd)

getLog :: MonadStore0 r [Logger]
getLog = gets snd

flushLog :: MonadStore0 r ()
flushLog = modify (\(a, _b) -> (a, []))

setData :: BSL.ByteString -> MonadStore0 r ()
setData x = modify (\(_, b) -> (Just x, b))

clearData :: MonadStore0 r ()
clearData = modify (\(_, b) -> (Nothing, b))
