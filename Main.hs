{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Version.Extra (readVersion)
import Distribution.Version
import Options.Applicative

import SimpleCmdArgs

import Diff

main :: IO ()
main =
  simpleCmdArgs Nothing "Cabal deps printer"
    "Tool that outputs depends lists for Cabal packages" $
    subcommands
    [ Subcommand "diff" "Diff .cabal files of package versions" $
      diffCmd <$> strArg "PKG" <*> versionArg <*> versionArg
    ]
  where
    versionArg :: Parser Version
    versionArg = argumentWith versionM "VERSION"

    versionM = maybeReader (Just . mkVersion' . readVersion)
