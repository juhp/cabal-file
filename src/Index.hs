{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Index (
  diffCmd,
  listPkg,
  listFiles,
  saveCabal,
  getMetaData,
  preferredVersions,
  latestPkg
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

-- FIXME structural diff of PackageDescription
-- FIXME revisions?
diffCmd :: String -> Version -> Version -> IO ()
diffCmd pkg v1 v2 =
  withTempDir $ \ tmpdir -> do
    setCurrentDirectory tmpdir
    let pkgid1 = PackageIdentifier (mkPackageName pkg) v1
        pkgid2 = PackageIdentifier (mkPackageName pkg) v2
    saveCabals pkgid1 pkgid2
    void $ cmdBool "diff" ["-u", showPkgId pkgid1 <.> "cabal", showPkgId pkgid2 <.> "cabal"]

--  pkgdesc <- finalPackageDescription [] cabal

saveCabals :: PackageIdentifier -> PackageIdentifier -> IO ()
saveCabals pkgId1 pkgId2 = do
  withLocalRepo $ \rep -> uncheckClientErrors $
      withIndex rep $ \ IndexCallbacks{..} -> do
        trusted <$> indexLookupCabal pkgId1 >>= BL.writeFile (showPkgId pkgId1 <.> "cabal")
        trusted <$> indexLookupCabal pkgId2 >>= BL.writeFile (showPkgId pkgId2 <.> "cabal")

saveCabal :: PackageIdentifier -> IO ()
saveCabal pkgId = do
  -- FIXME need to provide a version until have latest
  when ((pkgVersion pkgId) == nullVersion) $
    error' "Please specify the package version"
  withLocalRepo $ \rep -> uncheckClientErrors $
      withIndex rep $ \ IndexCallbacks{..} ->
        trusted <$> indexLookupCabal pkgId >>= BL.writeFile (showPkgId pkgId <.> "cabal")

getMetaData :: PackageIdentifier -> IO ()
getMetaData pkgId = do
  -- FIXME need to provide a version until have latest
  when ((pkgVersion pkgId) == nullVersion) $
    error' "Please specify the package version"
  withLocalRepo $ \rep -> uncheckClientErrors $
      withIndex rep $ \ IndexCallbacks{..} ->
        trusted <$> indexLookupMetadata pkgId >>= print

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

-- listPkg :: PackageName -> IO ()
-- listPkg pkg = do
--   withLocalRepo $ \rep -> uncheckClientErrors $ do
--     dir <- getDirectory rep
--     forM_ (directoryEntries dir) $ \ entry -> do
--       case third entry of
--         Just f -> case f of
--           Some (IndexPkgCabal pkgid) ->
--             when (pkgName pkgid == pkg) $
--                   putStrLn $ packageVersion pkgid
--           Some (IndexPkgMetadata _pkgid) -> return ()
--           Some (IndexPkgPrefs _prefer) -> return ()
--         Nothing -> return ()
--   where
--     third (_,_,c) = c

packageVersions :: PackageName -> IO [Version]
packageVersions pkgname = do
  withLocalRepo $ \rep -> uncheckClientErrors $ do
    dir <- getDirectory rep
    let pkg = unPackageName pkgname
    return $ sort . (mapMaybe (extractPkgVersion pkg . second)) $ directoryEntries dir
  where
    second (_,b,_) = b

    extractPkgVersion :: String -> IndexPath -> Maybe Version
    extractPkgVersion pkg path =
      if Path.takeExtension path == ".cabal" then
        let namever = (Path.toUnrootedFilePath . Path.unrootPath . Path.takeDirectory) path
        in if (takeDirectory namever == pkg)
           then Just $ mkVersion' . readVersion $ takeFileName namever
           else Nothing
      else Nothing

listPkg :: PackageName -> IO ()
listPkg pkgname = do
  versions <- packageVersions pkgname
  mapM_ (putStrLn . showVersion) versions

preferredVersions :: PackageName -> IO ()
preferredVersions pkgname = do
  withLocalRepo $ \rep -> uncheckClientErrors $
      withIndex rep $ \ IndexCallbacks{..} -> do
        mindexentry <- indexLookupFile (IndexPkgPrefs pkgname)
        case mindexentry of
          Nothing -> return ()
          Just indexentry -> BL.putStrLn $ indexEntryContent indexentry

listFiles :: IO ()
listFiles = do
  withLocalRepo $ \rep -> uncheckClientErrors $ do
    dir <- getDirectory rep
    forM_ (directoryEntries dir) $ \ entry -> do
      putStrLn $ (Path.toUnrootedFilePath . Path.unrootPath) $ second entry
  where
    second (_,b,_) = b

#if (defined(MIN_VERSION_simple_cmd) && MIN_VERSION_simple_cmd(0,1,4))
#else
error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' = errorWithoutStackTrace
#else
error' = error
#endif
#endif
