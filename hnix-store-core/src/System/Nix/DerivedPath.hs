module System.Nix.DerivedPath where

import Data.Text qualified as T
import Data.Set qualified as Set
import Data.Set (Set)
import System.Nix.StorePath

data OutputsSpec = OutputsSpec_All | OutputsSpec_Names (Set StorePathName)
  deriving (Eq, Ord, Show)

data DerivedPath = DerivedPath_Opaque StorePath | DerivedPath_Built StorePath OutputsSpec
  deriving (Eq, Ord, Show)

parseOutputsSpec :: Text -> Either String OutputsSpec
parseOutputsSpec t
  | t == "*" = Right OutputsSpec_All
  | otherwise = do
  names <- mapM makeStorePathName (T.splitOn "," t)
  if null names
    then Left "parseOutputsSpec: No names"
    else Right $ OutputsSpec_Names (Set.fromList names)

outputsSpecToText :: OutputsSpec -> Text
outputsSpecToText = \case
  OutputsSpec_All -> "*"
  OutputsSpec_Names ns -> T.intercalate "," (fmap unStorePathName (Set.toList ns))

parsePathWithOutputs :: StoreDir -> Text -> Either String DerivedPath
parsePathWithOutputs root p = case T.breakOn "!" p of
  (s,r) -> if T.null r
    then DerivedPath_Opaque <$> parsePathText root s
    else DerivedPath_Built <$> parsePathText root s <*> parseOutputsSpec r

derivedPathToText :: StoreDir -> DerivedPath -> Text
derivedPathToText root = \case
  DerivedPath_Opaque p -> storePathToText root p
  DerivedPath_Built p os -> storePathToText root p <> "!" <> outputsSpecToText os
