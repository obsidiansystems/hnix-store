{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}

module System.Nix.Store.Remote.Logger
  ( Logger(..)
  , Field(..)
  , processOutput
  , processOutput_
  )
where


import           Prelude                 hiding ( Last, get, put )
import qualified Control.Monad.State as S
import           Control.Monad.Except           ( throwError )
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Char8         as BSC

import           Network.Socket.ByteString      ( recv )

import           System.Nix.Store.Remote.Binary
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.MonadStore
import           System.Nix.Store.Remote.Socket


controlParser :: Get r Logger
controlParser = do
  ctrl <- get int
  case ctrl of
    StderrNext -> Next
      <$> get byteStringLen
    StderrRead -> Read
      <$> get int
    StderrWrite -> Write
      <$> get byteStringLen
    StderrLast -> pure Last
    StderrError -> flip Error
      <$> get byteStringLen
      <*> get int
    StderrStartActivity -> StartActivity
      <$> get int
      <*> get int
      <*> get int
      <*> get byteStringLen
      <*> get (list field)
      <*> get int
    StderrStopActivity -> StopActivity
      <$> get int
    StderrResult -> Result
      <$> get int
      <*> get int
      <*> get (list field)
    x          -> fail          $ "Invalid control message received:" <> show x

processOutput :: forall r. HasStoreSocket r => MonadStore0 r [Logger]
processOutput = do
  r <- ask
  go $ B.runGetIncremental $ runReaderT controlParser r
 where
  go :: B.Decoder Logger -> MonadStore0 r [Logger]
  go (B.Done _leftover _consumed ctrl) = do
    case ctrl of
      e@(Error _ _) -> pure [e]
      Last        -> pure [Last]
      Read _n     -> do
        (mdata, _) <- S.get
        case mdata of
          Nothing   -> throwError "No data to read provided"
          Just part -> do
            -- XXX: we should check/assert part size against n of (Read n)
            sockPutS lazyByteStringLen part
            clearData

        processOutput

      -- we should probably handle Read here as well
      x -> do
        next <- processOutput
        pure $ x : next
  go (B.Partial k) = do
    soc   <- asks storeSocket
    chunk <- liftIO (Just <$> recv soc 8)
    go (k chunk)

  go (B.Fail _leftover _consumed msg) = error $ fromString msg

processOutput_ :: forall r. HasStoreSocket r => MonadStore0 r ()
processOutput_ = do
  out <- processOutput
  modify (\(a, b) -> (a, b <> out))
  checkError

checkError :: MonadStore0 r ()
checkError = getError >>= \case
  (_, msg) : _ -> throwError $ BSC.unpack msg
  [] -> pass
