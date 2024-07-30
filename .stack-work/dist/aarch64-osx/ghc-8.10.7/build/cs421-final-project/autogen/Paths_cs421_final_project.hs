{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cs421_final_project (
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

bindir     = "/Users/yinchenliu/Documents/VS Code/Haskell_Project/cs421_final_project/.stack-work/install/aarch64-osx/dcf9f547815dc0639002ee0430820d5d42d210a94efc5547d32c3d52dd36c371/8.10.7/bin"
libdir     = "/Users/yinchenliu/Documents/VS Code/Haskell_Project/cs421_final_project/.stack-work/install/aarch64-osx/dcf9f547815dc0639002ee0430820d5d42d210a94efc5547d32c3d52dd36c371/8.10.7/lib/aarch64-osx-ghc-8.10.7/cs421-final-project-0.1.0.0-CIse2INvsat4kRDH7rLXIv-cs421-final-project"
dynlibdir  = "/Users/yinchenliu/Documents/VS Code/Haskell_Project/cs421_final_project/.stack-work/install/aarch64-osx/dcf9f547815dc0639002ee0430820d5d42d210a94efc5547d32c3d52dd36c371/8.10.7/lib/aarch64-osx-ghc-8.10.7"
datadir    = "/Users/yinchenliu/Documents/VS Code/Haskell_Project/cs421_final_project/.stack-work/install/aarch64-osx/dcf9f547815dc0639002ee0430820d5d42d210a94efc5547d32c3d52dd36c371/8.10.7/share/aarch64-osx-ghc-8.10.7/cs421-final-project-0.1.0.0"
libexecdir = "/Users/yinchenliu/Documents/VS Code/Haskell_Project/cs421_final_project/.stack-work/install/aarch64-osx/dcf9f547815dc0639002ee0430820d5d42d210a94efc5547d32c3d52dd36c371/8.10.7/libexec/aarch64-osx-ghc-8.10.7/cs421-final-project-0.1.0.0"
sysconfdir = "/Users/yinchenliu/Documents/VS Code/Haskell_Project/cs421_final_project/.stack-work/install/aarch64-osx/dcf9f547815dc0639002ee0430820d5d42d210a94efc5547d32c3d52dd36c371/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cs421_final_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cs421_final_project_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cs421_final_project_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cs421_final_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cs421_final_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cs421_final_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
