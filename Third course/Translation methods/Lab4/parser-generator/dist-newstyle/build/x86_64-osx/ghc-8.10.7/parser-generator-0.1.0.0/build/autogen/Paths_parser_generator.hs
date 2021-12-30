{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_parser_generator (
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

bindir     = "/Users/michael/.cabal/bin"
libdir     = "/Users/michael/.cabal/lib/x86_64-osx-ghc-8.10.7/parser-generator-0.1.0.0-inplace"
dynlibdir  = "/Users/michael/.cabal/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/michael/.cabal/share/x86_64-osx-ghc-8.10.7/parser-generator-0.1.0.0"
libexecdir = "/Users/michael/.cabal/libexec/x86_64-osx-ghc-8.10.7/parser-generator-0.1.0.0"
sysconfdir = "/Users/michael/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "parser_generator_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "parser_generator_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "parser_generator_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "parser_generator_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "parser_generator_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "parser_generator_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
