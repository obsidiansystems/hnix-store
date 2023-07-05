{-|
Description : Metadata about Nix store paths.
-}
module System.Nix.ValidPathInfo where

import           System.Nix.StorePath           ( StorePath
                                                , StorePathSet
                                                , ContentAddressableAddress
                                                )
import           System.Nix.Hash                ( SomeNamedDigest )
import           Data.Time                      ( UTCTime )
import System.Nix.Internal.Signature ( NarSignature )
import qualified Crypto.Hash as C
import System.Nix.Internal.ContentAddressed

-- | Metadata about a 'StorePath'
data ValidPathInfo = ValidPathInfo
  { -- | The path to the derivation file that built this path, if any
    -- and known.
    deriverPath :: !(Maybe StorePath)
  , -- TODO should this be optional?
    -- | The hash of the nar serialization of the path.
    narHash :: !(C.Digest C.SHA256)
  , -- | The paths that this path directly references
    references :: !StorePathSet
  , -- | When was this path registered valid in the store?
    registrationTime :: !UTCTime
  , -- | The size of the nar serialization of the path, in bytes.
    narBytes :: !(Maybe Word64)
  , -- | How much we trust this path.
    ultimate :: !Ultimate
  , -- | A set of cryptographic attestations of this path's validity.
    --
    -- There is no guarantee from this type alone that these
    -- signatures are valid.
    sigs :: !(Set NarSignature)
  , -- | Whether and how this store path is content-addressable.
    --
    -- There is no guarantee from this type alone that this address
    -- is actually correct for this store path.
    contentAddressableAddress :: !(Maybe ContentAddress)
  }

-- | Where was this path gotten
data Ultimate
  = -- | It was built locally and thus ultimately trusted
    BuiltLocally
  | -- | It was built elsewhere (and substituted or similar) and so
    -- is less trusted
    BuiltElsewhere
   deriving (Show, Eq, Ord)
