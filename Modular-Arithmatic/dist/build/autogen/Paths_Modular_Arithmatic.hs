module Paths_Modular_Arithmatic (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/lyndon/.cabal/bin"
libdir     = "/Users/lyndon/.cabal/lib/Modular-Arithmatic-0.1/ghc-6.12.3"
datadir    = "/Users/lyndon/.cabal/share/Modular-Arithmatic-0.1"
libexecdir = "/Users/lyndon/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "Modular_Arithmatic_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "Modular_Arithmatic_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "Modular_Arithmatic_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "Modular_Arithmatic_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
