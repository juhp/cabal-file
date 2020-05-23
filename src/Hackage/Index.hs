{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Hackage.Index (
  getCabal,
  getCabals,
  withCabalFile,
  getMetadata,
  indexFiles,
  latestVersion,
  packageVersions,
  preferredVersions,
  getPackageDescription,
  getPackageDescription'
  ) where

-- provided by simple-cmd-args 0.1.3
--import Control.Applicative ((<|>))
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Data.Maybe
import Data.Version.Extra (readVersion)
import System.Directory
import System.FilePath
import System.IO.Extra (withTempDir)

--import Distribution.PackageDescription (Library(..), exeName, setupDepends)
--import Distribution.Pretty
--import Distribution.Simple.BuildToolDepends (getAllToolDependencies)
--import Distribution.Types.ExeDependency
import Distribution.Version
#if MIN_VERSION_Cabal(3,0,0)
#else
  hiding (showVersion)
#endif
import Hackage.Security.Client
import qualified Hackage.Security.Client.Repository.Local as Local
import qualified Hackage.Security.Util.Path as Path
--import Hackage.Security.Util.Some
import qualified Hackage.Security.Client.Repository.Cache as Cache
import Hackage.Security.Util.Pretty

import SimpleCabal
import SimpleCmd

getCabal  :: PackageIdentifier -> IO BL.ByteString
getCabal pkgid = do
  -- FIXME need to provide a version until have latest
  when (pkgVersion pkgid == nullVersion) $
    error' "Please specify the package version"
  withLocalRepo $ \rep -> uncheckClientErrors $
    withIndex rep $ \ IndexCallbacks{..} ->
    trusted <$> indexLookupCabal pkgid

withCabalFile :: PackageIdentifier -> (FilePath -> IO a) -> IO a
withCabalFile pkgid act =
  withTempDir $ \ tmpdir -> do
    bs <- getCabal pkgid
    let filepath = tmpdir </> showPkgId pkgid <.> "cabal"
    BL.writeFile filepath bs
    act filepath

getCabals  :: PackageIdentifier -> PackageIdentifier
           -> IO (BL.ByteString, BL.ByteString)
getCabals pkgid1 pkgid2 = do
  -- FIXME need to provide a version until have latest
  when (pkgVersion pkgid1 == nullVersion ||
        pkgVersion pkgid2 == nullVersion) $
    error' "Please specify package version(s)"
  withLocalRepo $ \rep -> uncheckClientErrors $
    withIndex rep $ \ IndexCallbacks{..} -> do
    bs1 <- trusted <$> indexLookupCabal pkgid1
    bs2 <- trusted <$> indexLookupCabal pkgid2
    return (bs1,bs2)

getMetadata :: PackageIdentifier -> IO Targets
getMetadata pkgid = do
  -- FIXME need to provide a version until have latest
  when (pkgVersion pkgid == nullVersion) $
    error' "Please specify the package version"
  withLocalRepo $ \rep -> uncheckClientErrors $
      withIndex rep $ \ IndexCallbacks{..} ->
        trusted <$> indexLookupMetadata pkgid

getPackageDescription :: PackageIdentifier -> IO (Maybe PackageDescription)
getPackageDescription pkgid =
#if (defined(MIN_VERSION_simple_cabal) && MIN_VERSION_simple_cabal(0,1,2))
  do
  cabal <- getCabal pkgid
  parseFinalPackageDescription [] $ BL.toStrict cabal
#else
  Just <$> withCabalFile pkgid (finalPackageDescription [])
#endif

getPackageDescription' :: PackageIdentifier -> IO PackageDescription
getPackageDescription' pkgid = do
  mfpd <- getPackageDescription pkgid
  maybe (error' "Failed to parse cabal file") return mfpd

withLocalRepo :: (Repository Local.LocalFile -> IO a) -> IO a
withLocalRepo action = do
  home <- getHomeDirectory
  localrepo <- (Path.makeAbsolute . Path.fromFilePath) (home </> ".cabal")
  localcache <- (Path.makeAbsolute . Path.fromFilePath) (home </> ".cabal/packages/hackage.haskell.org")
  Local.withRepository localrepo (cache localcache) hackageRepoLayout hackageIndexLayout logTUF action
  where
    -- FIXME could also support 00-index
    cache localcache = Cache.Cache {
        Cache.cacheRoot   = localcache
      , Cache.cacheLayout = cabalCacheLayout
        { cacheLayoutIndexTar   = Path.rootPath $ Path.fragment "01-index.tar"
        , cacheLayoutIndexIdx   = Path.rootPath $ Path.fragment "01-index.tar.idx"
        , cacheLayoutIndexTarGz = Path.rootPath $ Path.fragment "01-index.tar.gz"}
    }

    logTUF msg = putStrLn $ "# " ++ pretty msg

packageVersions :: PackageName -> IO [Version]
packageVersions pkgname =
  withLocalRepo $ \rep -> uncheckClientErrors $ do
    dir <- getDirectory rep
    let pkg = unPackageName pkgname
    return $ sort . mapMaybe (extractPkgVersion pkg . second) $ directoryEntries dir
  where
    second (_,b,_) = b

    extractPkgVersion :: String -> IndexPath -> Maybe Version
    extractPkgVersion pkg path =
      if Path.takeExtension path == ".cabal" then
        let namever = (Path.toUnrootedFilePath . Path.unrootPath . Path.takeDirectory) path
        in if takeDirectory namever == pkg
           then Just $ mkVersion' . readVersion $ takeFileName namever
           else Nothing
      else Nothing

preferredVersions :: PackageName -> IO (Maybe BL.ByteString)
preferredVersions pkgname =
  withLocalRepo $ \rep -> uncheckClientErrors $
    withIndex rep $ \ IndexCallbacks{..} ->
    fmap indexEntryContent <$> indexLookupFile (IndexPkgPrefs pkgname)

indexFiles :: IO [String]
indexFiles =
  withLocalRepo $ \rep -> uncheckClientErrors $ do
    dir <- getDirectory rep
    return $ map dirEntryPath (directoryEntries dir)
  where
    second (_,b,_) = b

    dirEntryPath = Path.toUnrootedFilePath . Path.unrootPath . second
-- FIXME: take preferred-versions into account
latestVersion :: PackageName -> IO (Maybe Version)
latestVersion pkgname = do
  versions <- packageVersions pkgname
  if null versions then return Nothing
    else return $ Just $ last versions

#if (defined(MIN_VERSION_simple_cmd) && MIN_VERSION_simple_cmd(0,1,4))
#else
error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' = errorWithoutStackTrace
#else
error' = error
#endif
#endif
