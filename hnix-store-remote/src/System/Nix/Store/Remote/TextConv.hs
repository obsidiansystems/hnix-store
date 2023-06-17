{-# language RecordWildCards     #-}
module System.Nix.Store.Remote.TextConv where

import qualified Data.Text.Lazy.Encoding       as TL
import qualified Data.ByteString.Lazy          as BSL

bsToText :: ByteString -> Text
bsToText = decodeUtf8

textToBS :: Text -> ByteString
textToBS = encodeUtf8

bslToText :: BSL.ByteString -> Text
bslToText = toText . TL.decodeUtf8

textToBSL :: Text -> BSL.ByteString
textToBSL = TL.encodeUtf8 . toLText
