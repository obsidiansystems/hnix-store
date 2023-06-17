module TextConv where

import           Data.Text.Arbitrary ()
import           System.Nix.Store.Remote.TextConv
import           Test.Tasty.QuickCheck

prop_TextToBSLRoundtrip :: Text -> Property
prop_TextToBSLRoundtrip x = bslToText (textToBSL x) === x

prop_TextToBSRoundtrip :: Text -> Property
prop_TextToBSRoundtrip x = bsToText (textToBS x) === x
