-- | Discover installed GHCs.

module DiscoverGHCs where

import Control.Exception
  ( evaluate )  
import System.FilePath
  ( getSearchPath, stripExtension, takeFileName )
import System.FilePath.Find
  ( find, anyPerms, depth, fileName, (<=?), (~~?), (&&?) )
import System.IO.Silently
  ( hSilence )

import Util

import Options
import Types

-- | Discover executables ghc-VERSION on the PATH.

discoverGHCs :: Options -> IO [CompilerVersion]
discoverGHCs opts = do

  -- Get PATH.
  path <- getSearchPath

  chatLines opts $ "PATH:" : map ("- " ++) path

  -- Look for ghc-* executables on PATH.
  ghcs <- hSilence [stderr] $ do -- Silence warnings about broken symlinks produced by @find@.
    evaluate =<< do              -- Needed under Windows, otherwise warnings are raised too lazily
                                 -- and not caught by @hSilence@.
      concat <$> mapM (find recursionP filterP) path

  chatLines opts $ "GHCs on PATH:" : map ("- " ++) ghcs

  -- Parse results into @[CompilerVersion]@.
  let versions = sort $ mapMaybe (parseCompilerString . stripExe . takeFileName) ghcs

  chatLines opts $ "Installed GHCs:" :
    map (("- " ++) . printNumericVersion . toNumericVersion) versions

  return versions

  where
  -- Never recurse into subdirectories.
  recursionP = depth <=? 0
  -- Find executables named @ghc-[0-9]*@.
  filterP = fileName ~~? "ghc-[0-9]*" &&? anyPerms 0111
  -- Remove a potential "exe" extension.
  stripExe file = fromMaybe file $ stripExtension "exe" file
