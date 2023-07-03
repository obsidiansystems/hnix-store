{-# language RecordWildCards      #-}
{-# language ScopedTypeVariables  #-}
module System.Nix.Store.Remote.Socket where

import           Prelude                 hiding ( putText )

import qualified Data.Binary.Get               as B
import qualified Data.Binary.Put               as B
import qualified Data.ByteString.Char8         as BSC

import           Network.Socket                 ( Socket )
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )

import qualified System.Nix.Store.Remote.Binary as RB

newtype DecodingError = DecodingError String
  deriving (Eq, Ord, Show)

class HasStoreSocket r where
  storeSocket :: r -> Socket

instance HasStoreSocket Socket where
  storeSocket = id

genericIncremental :: (MonadIO m) => m (Maybe ByteString) -> B.Get a -> m (Either String a)
genericIncremental getsome parser = go decoder
 where
  decoder = B.runGetIncremental parser
  go (B.Done _leftover _consumed x  ) = pure $ Right x
  go (B.Partial k                   ) = do
    chunk <- getsome
    go (k chunk)
  go (B.Fail _leftover _consumed msg) = pure $ Left msg

sockTryGet
  :: forall r m a
  .  (MonadReader r m, MonadIO m, HasStoreSocket r)
  => RB.Get r a
  -> m (Either String a)
sockTryGet g = do
  r <- ask
  genericIncremental sockGet8 $ runReaderT g r
 where
  sockGet8 :: m (Maybe BSC.ByteString)
  sockGet8 = do
    soc <- asks storeSocket
    liftIO $ Just <$> recv soc 8

sockGetS :: (MonadIO m, MonadReader r m, HasStoreSocket r) => RB.Serializer r a -> m a
sockGetS s = sockGet (RB.get s)

sockGet :: (MonadReader r m, MonadIO m, HasStoreSocket r) => RB.Get r a -> m a
sockGet = (either (liftIO . fail) pure) <=< sockTryGet

sockPut :: (MonadReader r m, MonadIO m, HasStoreSocket r) => RB.Put r -> m ()
sockPut p = do
  r <- ask
  liftIO $ sendAll (storeSocket r) $ toStrict $ B.runPut $ runReaderT p r

sockPutS :: (MonadIO m, MonadReader r m, HasStoreSocket r) => RB.Serializer r a -> a -> m ()
sockPutS s x = sockPut (RB.put s x)
