{-# language AllowAmbiguousTypes #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes          #-}
{-# language DataKinds           #-}

module System.Nix.Store.Remote.Parsers
  ( parseContentAddress
  )
where

import Data.Dependent.Sum
import Data.Attoparsec.ByteString.Char8
import System.Nix.Hash
import Crypto.Hash

-- | Parse `ContentAddressableAddress` from `ByteString`
parseContentAddress
  :: ByteString -> Either String ContentAddress
parseContentAddress =
  Data.Attoparsec.ByteString.Char8.parseOnly contentAddressParser

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
  decodeUtf8 <$> ("sha256" <|> "sha512" <|> "sha1" <|> "md5") <* (":" <|> "-")

parseHash :: Parser Text
parseHash = decodeUtf8 <$> takeWhile1 (/= ':')
