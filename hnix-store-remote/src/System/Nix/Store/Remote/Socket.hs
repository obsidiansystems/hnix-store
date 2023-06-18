{-# language RecordWildCards      #-}
{-# language ScopedTypeVariables  #-}
module System.Nix.Store.Remote.Socket where

import           Prelude                 hiding ( putText )

import qualified Data.Binary.Get               as B
import qualified Data.Binary.Put               as B
import qualified Data.ByteString.Char8         as BSC

import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )

import qualified System.Nix.Store.Remote.Binary as RB
import           System.Nix.Store.Remote.MonadStore

genericIncremental :: (MonadIO m) => m (Maybe ByteString) -> B.Get a -> m a
genericIncremental getsome parser = go decoder
 where
  decoder = B.runGetIncremental parser
  go (B.Done _leftover _consumed x  ) = pure x
  go (B.Partial k                   ) = do
    chunk <- getsome
    go (k chunk)
  go (B.Fail _leftover _consumed msg) = error $ fromString msg

getSocketIncremental :: forall r a. HasStoreSocket r => RB.Get r a -> MonadStore0 r a
getSocketIncremental g = do
  r <- ask
  genericIncremental sockGet8 $ runReaderT g r
 where
  sockGet8 :: MonadStore0 r (Maybe BSC.ByteString)
  sockGet8 = do
    soc <- asks storeSocket
    liftIO $ Just <$> recv soc 8

sockPut :: HasStoreSocket r => RB.Put r -> MonadStore0 r ()
sockPut p = do
  r <- ask
  liftIO $ sendAll (storeSocket r) $ toStrict $ B.runPut $ runReaderT p r

sockGet :: HasStoreSocket r => RB.Get r a -> MonadStore0 r a
sockGet = getSocketIncremental
