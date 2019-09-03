{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hedgehog_talk (
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
version = Version [0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/chessai/.cabal/bin"
libdir     = "/home/chessai/.cabal/lib/x86_64-linux-ghc-8.6.5/hedgehog-talk-0.1-inplace"
dynlibdir  = "/home/chessai/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/chessai/.cabal/share/x86_64-linux-ghc-8.6.5/hedgehog-talk-0.1"
libexecdir = "/home/chessai/.cabal/libexec/x86_64-linux-ghc-8.6.5/hedgehog-talk-0.1"
sysconfdir = "/home/chessai/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hedgehog_talk_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hedgehog_talk_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hedgehog_talk_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hedgehog_talk_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hedgehog_talk_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hedgehog_talk_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
