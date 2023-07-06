module System.Nix.ContentAddress where

import Crypto.Hash
import Data.Attoparsec.Text
import Data.Dependent.Sum
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder ( Builder )
import Data.Text.Lazy.Builder qualified as TL
import System.Nix.Hash

data FileIngestionMethod
  = Flat
  | FileRecursive

data ContentAddressMethod
  = FileIngestionMethod !FileIngestionMethod
-- ^ The path was added to the store via makeFixedOutputPath or
  -- addToStore. It is addressed according to some hash algorithm
  -- applied to the nar serialization via some 'NarHashMode'.
  | TextIngestionMethod
  -- ^ The path is a plain file added via makeTextPath or
  -- addTextToStore. It is addressed according to a sha256sum of the
  -- file contents.

-- | An address for a content-addressable store path, i.e. one whose
-- store path hash is purely a function of its contents (as opposed to
-- paths that are derivation outputs, whose hashes are a function of
-- the contents of the derivation file instead).
--
-- For backwards-compatibility reasons, the same information is
-- encodable in multiple ways, depending on the method used to add the
-- path to the store. These unfortunately result in separate store
-- paths.
data ContentAddress = ContentAddress
  ContentAddressMethod
  (DSum HashAlgo Digest)


-- | Marshall `ContentAddressableAddress` to `Text`
-- in form suitable for remote protocol usage.
buildContentAddress :: ContentAddress -> Text
buildContentAddress =
  TL.toStrict . TL.toLazyText . contentAddressBuilder

contentAddressBuilder :: ContentAddress -> Builder
contentAddressBuilder (ContentAddress method digest) = case method of
  TextIngestionMethod -> "text:" <> digestBuilder digest
  FileIngestionMethod r -> "fixed:"
    <> fileIngestionMethodBuilder r
    <> digestBuilder digest

fileIngestionMethodBuilder :: FileIngestionMethod -> Builder
fileIngestionMethodBuilder = \case
  Flat -> ""
  FileRecursive -> "r:"

digestBuilder :: DSum HashAlgo Digest -> Builder
digestBuilder (a :=> d) =
  TL.fromText (algoToText a)
  <> TL.fromText (encodeDigestWith NixBase32 d)

-- | Parse `ContentAddressableAddress` from `ByteString`
parseContentAddress
  :: Text -> Either String ContentAddress
parseContentAddress =
  Data.Attoparsec.Text.parseOnly contentAddressParser

-- | Parser for content addressable field
contentAddressParser :: Parser ContentAddress
contentAddressParser = do
  method <- parseContentAddressMethod
  digest <- parseTypedDigest
  case digest of
    Left e -> fail e
    Right x -> return $ ContentAddress method x

parseContentAddressMethod :: Parser ContentAddressMethod
parseContentAddressMethod =
      TextIngestionMethod <$ "text:"
  <|> FileIngestionMethod <$ "fixed:" <*> (FileRecursive <$ "r:" <|> pure Flat)

parseTypedDigest :: Parser (Either String (DSum HashAlgo Digest))
parseTypedDigest = mkNamedDigest <$> parseHashType <*> parseHash

parseHashType :: Parser Text
parseHashType =
  ("sha256" <|> "sha512" <|> "sha1" <|> "md5") <* (":" <|> "-")

parseHash :: Parser Text
parseHash = takeWhile1 (/= ':')
