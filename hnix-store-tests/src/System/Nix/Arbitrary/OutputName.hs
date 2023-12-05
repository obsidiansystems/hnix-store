{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.OutputName where

import System.Nix.OutputName (OutputName)
import qualified Data.Text
import qualified System.Nix.OutputName

import Test.QuickCheck (Arbitrary(arbitrary), elements, listOf)

instance Arbitrary OutputName where
  arbitrary =
      either (error . show) id
    . System.Nix.OutputName.mkOutputName
    . Data.Text.pack <$> ((:) <$> s1 <*> listOf sn)
   where
    alphanum = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
    s1       = elements $ alphanum <> "+-_?="
    sn       = elements $ alphanum <> "+-._?="
