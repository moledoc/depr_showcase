{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_expenses_hs (
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

bindir     = "/home/utt/Documents/showcase/expenses-hs/.stack-work/install/x86_64-linux-tinfo6/7750ed37eebacff4dcdaa51982b332c0a90db905300ac8c28ff1844d5ddc2345/8.10.2/bin"
libdir     = "/home/utt/Documents/showcase/expenses-hs/.stack-work/install/x86_64-linux-tinfo6/7750ed37eebacff4dcdaa51982b332c0a90db905300ac8c28ff1844d5ddc2345/8.10.2/lib/x86_64-linux-ghc-8.10.2/expenses-hs-0.1.0.0-3yMbnib2TCS9y03SqWO8BU-expenses-hs"
dynlibdir  = "/home/utt/Documents/showcase/expenses-hs/.stack-work/install/x86_64-linux-tinfo6/7750ed37eebacff4dcdaa51982b332c0a90db905300ac8c28ff1844d5ddc2345/8.10.2/lib/x86_64-linux-ghc-8.10.2"
datadir    = "/home/utt/Documents/showcase/expenses-hs/.stack-work/install/x86_64-linux-tinfo6/7750ed37eebacff4dcdaa51982b332c0a90db905300ac8c28ff1844d5ddc2345/8.10.2/share/x86_64-linux-ghc-8.10.2/expenses-hs-0.1.0.0"
libexecdir = "/home/utt/Documents/showcase/expenses-hs/.stack-work/install/x86_64-linux-tinfo6/7750ed37eebacff4dcdaa51982b332c0a90db905300ac8c28ff1844d5ddc2345/8.10.2/libexec/x86_64-linux-ghc-8.10.2/expenses-hs-0.1.0.0"
sysconfdir = "/home/utt/Documents/showcase/expenses-hs/.stack-work/install/x86_64-linux-tinfo6/7750ed37eebacff4dcdaa51982b332c0a90db905300ac8c28ff1844d5ddc2345/8.10.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "expenses_hs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "expenses_hs_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "expenses_hs_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "expenses_hs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "expenses_hs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "expenses_hs_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
