{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_case4 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/adamberger/Library/Haskell/bin"
libdir     = "/Users/adamberger/Library/Haskell/ghc-8.2.2-x86_64/lib/case4-0.1.0.0"
dynlibdir  = "/Users/adamberger/Library/Haskell/ghc-8.2.2-x86_64/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/adamberger/Library/Haskell/share/ghc-8.2.2-x86_64/case4-0.1.0.0"
libexecdir = "/Users/adamberger/Library/Haskell/libexec/x86_64-osx-ghc-8.2.2/case4-0.1.0.0"
sysconfdir = "/Users/adamberger/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "case4_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "case4_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "case4_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "case4_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "case4_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "case4_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
