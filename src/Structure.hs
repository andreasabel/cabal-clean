-- {-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-deprecations #-}  -- versionTags

-- | Data structure for the `dist-newstyle/build` directory.
--
-- Example:
--
-- @
--   dist-newstyle/build/
--   └── x86_64-osx
--       ├── ghc-7.10.3
--       │   ├── BNFC-2.9.2
--       │   └── BNFC-2.9.3
--       ├── ghc-8.10.4
--       │   └── BNFC-2.9.2
--       ├── ghc-8.10.5
--       │   └── BNFC-2.9.3
--       └── ghc-9.0.1
--           ├── BNFC-2.9.2
--           └── BNFC-2.9.3
-- @
--
-- We organize the contents of the build directory
-- in the way we see fit:
--
-- * Level 1: package
-- * Level 2: package version
-- * Level 3: architecture
-- * Level 4: compiler version

module Structure where

import qualified Data.Map as Map
import Util

-- | The structure of the build directory.

newtype BuildTree = BuildTree
  { theBuildTree ::
      Map Package
       (Map PackageVersion
         (Map Arch
           (Map CompilerVersion FilePath)))
  }
  deriving Show

-- | The build directory is a map from 'Entry' to 'FilePath'.

data Entry = Entry
  { pkg    :: Package
  , pkgVer :: PackageVersion
  , arch   :: Arch
  , ghcVer :: CompilerVersion
  }

-- | We treat the architecture identifier as opaque.
type Arch = String

-- | A package is given by its name.
type Package = String

-- | A package version is a list of natural numbers.
type PackageVersion  = NumericVersion

-- | A GHC version is a list of natural numbers.
type CompilerVersion = NumericVersion

type NumericVersion = [Int]

-- * Loading the build tree from disc.

type Warnings = [String]

type M = WriterT Warnings IO

getBuildTree :: FilePath -> IO (BuildTree, Warnings)
getBuildTree root = runWriterT $ do

  -- Traverse architectures
  forMM (getSubdirectories root) $ \ arch -> do
    let archroot = root </> arch

    -- Traverse compilers
    forMM (getSubdirectories archroot) $ \ hc -> do
      let hcroot = archroot </> hc
      case parseCompilerString hc of
        Nothing -> warnIgnoring hcroot
        Just ghcVer -> do

          -- Traverse packages
          forMM (getSubdirectories hcroot) $ \ s -> do
            let pkgdir = hcroot </> s
            case parsePackageString s of
              Nothing -> warnIgnoring pkgdir
              Just (pkg, ver) -> do
                return $ singleton (Entry pkg ver arch ghcVer) pkgdir
  where
  warnIgnoring dir = do
    tell [ unwords ["Ignoring", dir] ]
    return mempty

-- | Precondition: argument must be a directory.

getSubdirectories :: MonadIO io => FilePath -> io [FilePath]
getSubdirectories root = liftIO $ do
  filterM (doesDirectoryExist . (root </>)) =<< listDirectory root

-- * Printing the build tree to the terminal.

-- | Print the build tree, coloring parts to keep in green, and parts to remove in red.

printBuildTree :: BuildTree -> IO ()
printBuildTree _ = return ()



-- * Managing the build directory structure.

instance Semigroup BuildTree where
  BuildTree t1 <> BuildTree t2 = BuildTree $
    Map.unionWith (Map.unionWith $ Map.unionWith $ Map.union) t1 t2

instance Monoid BuildTree where
  mempty  = BuildTree $ Map.empty
  mappend = (<>)

-- | A build tree with a single entry.

singleton :: Entry -> FilePath -> BuildTree
singleton (Entry pkg ver arch ghc) dir = BuildTree $
  Map.singleton pkg $ Map.singleton ver $ Map.singleton arch $ Map.singleton ghc dir

-- * Parsing directory names

parseEntry :: Arch -> CompilerString -> PackageString -> Maybe Entry
parseEntry arch hc s = do
  ghcVer   <- parseCompilerString hc
  (pkg, v) <- parsePackageString s
  return $ Entry pkg v arch ghcVer

type CompilerString = String

parseCompilerString :: CompilerString -> Maybe CompilerVersion
parseCompilerString s = do
  n <- findIndex (== '-') s
  case splitAt n s of
    ("ghc", _:v) -> parseVersionString v
    _ -> Nothing

type PackageString = String

parsePackageString :: PackageString -> Maybe (Package, PackageVersion)
parsePackageString s = do
  n <- findIndexEnd (== '-') s
  let (p, _:v) = splitAt n s
  (p,) <$> parseVersionString v

type VersionString = String

parseVersionString :: VersionString -> Maybe NumericVersion
parseVersionString = mapM readMaybe . splitWhen (== '.')
