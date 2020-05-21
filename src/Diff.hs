{-# LANGUAGE RecordWildCards #-}

module Diff (diffCmd) where

-- provided by simple-cmd-args 0.1.3
--import Control.Applicative ((<|>))
import Control.Monad
import qualified Data.ByteString.Lazy as BL
--import Data.List
--import Data.Maybe
import System.Directory
import System.FilePath
import System.IO.Extra (withTempDir)

--import Distribution.PackageDescription (Library(..), exeName, setupDepends)
--import Distribution.Pretty
--import Distribution.Simple.BuildToolDepends (getAllToolDependencies)
--import Distribution.Types.ExeDependency
import Distribution.Version
import Hackage.Security.Client
import qualified Hackage.Security.Client.Repository.Local as Local
import qualified Hackage.Security.Util.Path as Path
import qualified Hackage.Security.Client.Repository.Cache as Cache
import Hackage.Security.Util.Pretty

import SimpleCabal
import SimpleCmd


-- FIXME revisions?
diffCmd :: String -> Version -> Version -> IO ()
diffCmd pkg v1 v2 =
  withTempDir $ \ tmpdir -> do
    setCurrentDirectory tmpdir
    let pkgid1 = PackageIdentifier (mkPackageName pkg) v1
        pkgid2 = PackageIdentifier (mkPackageName pkg) v2
    saveCabal pkgid1
    saveCabal pkgid2
    void $ cmdBool "diff" ["-u", showPkgId pkgid1 <.> "cabal", showPkgId pkgid2 <.> "cabal"]

--  pkgdesc <- finalPackageDescription [] cabal

saveCabal :: PackageIdentifier -> IO ()
saveCabal pkgId = do
  home <- getHomeDirectory
  localrepo <- (Path.makeAbsolute . Path.fromFilePath) (home </> ".cabal")
  localcache <- (Path.makeAbsolute . Path.fromFilePath) (home </> ".cabal/packages/hackage.haskell.org")
  withLocalRepo localrepo localcache $ \rep -> uncheckClientErrors $
      withIndex rep $ \ IndexCallbacks{..} ->
        trusted <$> indexLookupCabal pkgId >>= BL.writeFile (showPkgId pkgId <.> "cabal")
  where
    withLocalRepo repo localcache =
        Local.withRepository repo
                             (cache localcache)
                             hackageRepoLayout
                             hackageIndexLayout
                             logTUF

    -- FIXME could also support 00-index
    cache localcache = Cache.Cache {
        Cache.cacheRoot   = localcache
      , Cache.cacheLayout = cabalCacheLayout
        { cacheLayoutIndexTar   = Path.rootPath $ Path.fragment "01-index.tar"
        , cacheLayoutIndexIdx   = Path.rootPath $ Path.fragment "01-index.tar.idx"
        , cacheLayoutIndexTarGz = Path.rootPath $ Path.fragment "01-index.tar.gz"}
    }

    logTUF msg = putStrLn $ "# " ++ pretty msg
