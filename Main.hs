{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Version.Extra (readVersion)
import Distribution.Version
import Options.Applicative

import SimpleCmdArgs

import Depends
import Diff

main :: IO ()
main =
  simpleCmdArgs Nothing "Cabal file tool"
    "cabal-file can access and compare .cabal files from the cache" $
    subcommands
    [ Subcommand "diff" "Diff .cabal files of package versions" $
      diffCmd <$> strArg "PKG" <*> versionArg <*> versionArg
    , Subcommand "depends" "Print package dependencies" $
      dependsCmd <$> switchWith 'q' "quiet" "No depends section headers"
      <*> detailOpt
      <*> optional depsOpt
      <*> optional (strArg "DIRorCABAL")
    ]
  where
    versionArg :: Parser Version
    versionArg = argumentWith versionM "VERSION"

    versionM = maybeReader (Just . mkVersion' . readVersion)

    detailOpt :: Parser Details
    detailOpt =
      flagWith' Detail 'd' "detail" "Show version ranges" <|>
      flagWith Normal Unique 'u' "unique" "Remove self and build-depends from other depends"

    depsOpt :: Parser Deps
    depsOpt =
      flagWith' Build 'b' "build" "Only build-depends" <|>
      flagWith' Setup 's' "setup" "Only setup-depends" <|>
      flagWith' Tool 't' "tool" "Only tool-build-depends" <|>
      flagWith' Legacy 'l' "legacy" "Only build-tools" <|>
      flagWith' CLib 'c' "clibs" "Only extra-libraries" <|>
      flagWith' PkgConfig 'p' "pkgconfig" "Only extra-libraries" <|>
      flagWith' NotBuild 'B' "not-build" "Hide build-depends"
