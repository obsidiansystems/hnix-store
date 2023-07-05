{-|
Description : Representation of Nix store paths.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module System.Nix.StorePath
  ( -- * Basic store path types
    StoreDir(..)
  , HasStoreDir(..)
  , getStoreDir
  , StorePath(..)
  , StorePathName(..)
  , StorePathSet
  , mkStorePathHashPart
  , StorePathHashPart(..)
  , NarHashMode(..)
  , -- * Manipulating 'StorePathName'
    makeStorePathName
  , validStorePathName
  , -- * Rendering out 'StorePath's
    storePathToFilePath
  , storePathToRawFilePath
  , storePathToText
  , storePathToNarInfo
  , -- * Parsing 'StorePath's
    parsePath
  , pathParser
  )
where

import Crypto.Hash (SHA256)
import Data.Attoparsec.Text.Lazy (Parser, (<?>))
import Data.Attoparsec.Text.Lazy qualified as Parser.Text.Lazy
import Data.ByteString.Char8 qualified as Bytes.Char8
import Data.Char qualified as Char
import Data.Text qualified as Text
import Relude.Unsafe qualified as Unsafe
import System.FilePath qualified as FilePath
import System.Nix.Internal.Base
import System.Nix.Internal.Base32 qualified as Nix.Base32
import System.Nix.Hash

-- | A path in a Nix store.
--
-- From the Nix thesis: A store path is the full path of a store
-- object. It has the following anatomy: storeDir/hashPart-name.
--
-- The store directory is *not* included, and must be known from the
-- context. This matches modern C++ Nix, and also represents the fact
-- that store paths for different store directories cannot be mixed.
data StorePath = StorePath
  { -- | The 160-bit hash digest reflecting the "address" of the name.
    -- Currently, this is a truncated SHA256 hash.
    storePathHash :: !StorePathHashPart
  , -- | The (typically human readable) name of the path. For packages
    -- this is typically the package name and version (e.g.
    -- hello-1.2.3).
    storePathName :: !StorePathName
  }
  deriving (Eq, Ord, Show)

instance Hashable StorePath where
  hashWithSalt s StorePath{..} =
    s `hashWithSalt` storePathHash `hashWithSalt` storePathName

-- | The name portion of a Nix path.
--
-- 'unStorePathName' must only contain a-zA-Z0-9+._?=-, can't start
-- with a -, and must have at least one character (i.e. it must match
-- 'storePathNameRegex').
newtype StorePathName = StorePathName
  { -- | Extract the contents of the name.
    unStorePathName :: Text
  } deriving (Eq, Hashable, Ord, Show)

-- | The hash algorithm used for store path hashes.
newtype StorePathHashPart = StorePathHashPart ByteString
  deriving (Eq, Hashable, Ord, Show)

mkStorePathHashPart :: ByteString -> StorePathHashPart
mkStorePathHashPart = coerce . mkStorePathHash @SHA256

-- | A set of 'StorePath's.
type StorePathSet = HashSet StorePath

-- | Schemes for hashing a Nix archive.
--
-- For backwards-compatibility reasons, there are two different modes
-- here, even though 'Recursive' should be able to cover both.
data NarHashMode
  = -- | Require the nar to represent a non-executable regular file.
    RegularFile
  | -- | Hash an arbitrary nar, including a non-executable regular
    -- file if so desired.
    Recursive

makeStorePathName :: Text -> Either String StorePathName
makeStorePathName n =
  if validStorePathName n
    then pure $ StorePathName n
    else Left $ reasonInvalid n

reasonInvalid :: Text -> String
reasonInvalid n
  | n == ""          = "Empty name"
  | Text.length n > 211 = "Path too long"
  | Text.head n == '.'  = "Leading dot"
  | otherwise        = "Invalid character"

validStorePathName :: Text -> Bool
validStorePathName n =
  n /= ""
  && Text.length n <= 211
  && Text.head n /= '.'
  && Text.all validStorePathNameChar n

validStorePathNameChar :: Char -> Bool
validStorePathNameChar c =
  any ($ c)
    [ Char.isAsciiLower -- 'a'..'z', isAscii..er probably faster then putting it out
    , Char.isAsciiUpper -- 'A'..'Z'
    , Char.isDigit
    , (`elem` ("+-._?=" :: String))
    ]

-- | Copied from @RawFilePath@ in the @unix@ package, duplicated here
-- to avoid the dependency.
type RawFilePath = ByteString

-- | The path to the store dir
--
-- Many operations need to be parameterized with this, since store paths
-- do not know their own store dir by design.
newtype StoreDir = StoreDir {
    unStoreDir :: RawFilePath
  } deriving (Eq, Hashable, Ord, Show)

class HasStoreDir r where
  storeDir :: r -> StoreDir

getStoreDir :: (HasStoreDir r, MonadReader r m) => m StoreDir
getStoreDir = asks storeDir

-- | Render a 'StorePath' as a 'RawFilePath'.
storePathToRawFilePath :: StoreDir -> StorePath -> RawFilePath
storePathToRawFilePath sDir StorePath{..} =
  unStoreDir sDir <> "/" <> hashPart <> "-" <> name
 where
  hashPart = encodeUtf8 $ encodeWith NixBase32 $ coerce storePathHash
  name     = encodeUtf8 $ unStorePathName storePathName

-- | Render a 'StorePath' as a 'FilePath'.
storePathToFilePath :: StoreDir -> StorePath -> FilePath
storePathToFilePath sDir = Bytes.Char8.unpack . storePathToRawFilePath sDir

-- | Render a 'StorePath' as a 'Text'.
storePathToText :: StoreDir -> StorePath -> Text
storePathToText sDir = toText . Bytes.Char8.unpack . storePathToRawFilePath sDir

-- | Build `narinfo` suffix from `StorePath` which
-- can be used to query binary caches.
storePathToNarInfo :: StorePath -> Bytes.Char8.ByteString
storePathToNarInfo StorePath{..} =
  encodeUtf8 $ encodeWith NixBase32 (coerce storePathHash) <> ".narinfo"

-- | Parse `StorePath` from `Bytes.Char8.ByteString`, checking
-- that store directory matches `expectedRoot`.
parsePath :: StoreDir -> Bytes.Char8.ByteString -> Either String StorePath
parsePath expectedRoot x =
  let
    (rootDir, fname) = FilePath.splitFileName . Bytes.Char8.unpack $ x
    (storeBasedHashPart, namePart) = Text.breakOn "-" $ toText fname
    storeHash = decodeWith NixBase32 storeBasedHashPart
    name = makeStorePathName . Text.drop 1 $ namePart
    --rootDir' = dropTrailingPathSeparator rootDir
    -- cannot use ^^ as it drops multiple slashes /a/b/// -> /a/b

    rootDir' = Unsafe.init rootDir
    expectedRootS = Bytes.Char8.unpack (unStoreDir expectedRoot)
  in if expectedRootS /= rootDir'
      then Left $ "Root store dir mismatch, expected" <> expectedRootS <> "got" <> rootDir'
      else StorePath <$> coerce storeHash <*> name

pathParser :: StoreDir -> Parser StorePath
pathParser expectedRoot = do
  let expectedRootS = Bytes.Char8.unpack (unStoreDir expectedRoot)

  _ <-
    Parser.Text.Lazy.string (toText expectedRootS)
      <?> "Store root mismatch" -- e.g. /nix/store

  _ <- Parser.Text.Lazy.char '/'
      <?> "Expecting path separator"

  digest <-
    decodeWith NixBase32
    <$> Parser.Text.Lazy.takeWhile1 (`elem` Nix.Base32.digits32)
      <?> "Invalid Base32 part"

  _  <- Parser.Text.Lazy.char '-' <?> "Expecting dash (path name separator)"

  c0 <-
    Parser.Text.Lazy.satisfy (\c -> c /= '.' && validStorePathNameChar c)
      <?> "Leading path name character is a dot or invalid character"

  rest <-
    Parser.Text.Lazy.takeWhile validStorePathNameChar
      <?> "Path name contains invalid character"

  let name = makeStorePathName $ Text.cons c0 rest

  either
    fail
    pure
    (StorePath <$> coerce digest <*> name)
