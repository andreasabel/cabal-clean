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
-- * Level 4: major compiler version
-- * Level 5: minor compiler version

module Structure where

import qualified Data.Map as Map
import Util

-- | The structure of the build directory.

newtype BuildTree = BuildTree
  { theBuildTree ::
      Map Package
       (Map PackageVersion
         (Map Arch
           (Map MajorVersion
              (Map MinorVersion Entry))))
  }
  deriving Show

-- | The build directory is a map from 'Key' to 'Entry'.

data Key = Key
  { pkg    :: Package
  , pkgVer :: PackageVersion
  , arch   :: Arch
  , ghcMaj :: MajorVersion
  , ghcMin :: MinorVersion
  }

data Entry = Entry
  { dir      :: FilePath
  , obsolete :: Bool
  } deriving Show

-- | We treat the architecture identifier as opaque.
type Arch = String

-- | A package is given by its name.
type Package = String

-- | A package version is a list of natural numbers.
type PackageVersion  = NumericVersion

-- | A GHC major version is a list of natural numbers.
type MajorVersion = NumericVersion

-- | A GHC minor version is a list of natural numbers.
type MinorVersion = NumericVersion

type CompilerVersion = (MajorVersion, MinorVersion)

type NumericVersion = [Int]

-- * Loading the build tree from disc.

type Warnings = [String]

-- | Load the build tree from e.g. @dist-newstyle/build@.
--
-- Generate warning for subdirectories we cannot parse,
-- but ignore them otherwise.

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
        Just (major, minor) -> do

          -- Traverse packages
          forMM (getSubdirectories hcroot) $ \ s -> do
            let pkgdir = hcroot </> s
            case parsePackageString s of
              Nothing -> warnIgnoring pkgdir
              Just (pkg, ver) -> do
                return $ singleton (Key pkg ver arch major minor) pkgdir
  where
  warnIgnoring dir = do
    tell [ unwords ["Ignoring", dir] ]
    return mempty

-- | Precondition: argument must be a directory.

getSubdirectories :: MonadIO io => FilePath -> io [FilePath]
getSubdirectories root = liftIO $ do
  filterM (doesDirectoryExist . (root </>)) =<< listDirectory root

-- * Mark obsolete entries of the build tree.

-- | Mark entries that are superseded by either a new package version
-- or a new compiler minor version.

markObsolete :: BuildTree -> BuildTree
markObsolete (BuildTree t) = BuildTree $
  flip fmap t $ modifyDesc $ \case
    [] -> []
    (ver, m) : vms ->
      (ver, flip (fmap . fmap) m $ modifyDesc $ \case
        [] -> []
        me : mes -> me : map (second markEntryObsolete) mes
      ) : map (second $ fmap $ fmap $ fmap markEntryObsolete) vms

  where
  modifyDesc f = Map.fromDescList . f . Map.toDescList
  -- mapDesc f = Map.fromDescList . map (second f) . Map.toDescList

markEntryObsolete :: Entry -> Entry
markEntryObsolete (Entry dir _) = Entry dir True

-- | Remove directories marked as obsolete.

removeObsolete :: BuildTree -> IO ()
removeObsolete = foldMapEntry $ \ (Entry dir obsolete) -> do
  when obsolete $ do
    removeDirectoryRecursive dir

-- * Printing the build tree to the terminal.

-- | Print the build tree, coloring parts to keep in green, and parts to remove in red.

printBuildTree :: BuildTree -> IO ()
printBuildTree = foldMapEntry $ \ (Entry dir obsolete) -> do
  (exitcode, stdout, _stderr) <- readProcessWithExitCode "du" ["-hs", dir] ""
  let s = if exitcode == ExitSuccess then stdout else dir ++ "\n"
  putStr $ colorize obsolete s
  where
  colorize True  = color Red   -- . (++ " [DELETE]") -- wrong line break
  colorize False = color Green


-- * Mathematics of the build directory structure.

instance Semigroup BuildTree where
  BuildTree t1 <> BuildTree t2 = BuildTree $
    Map.unionWith (Map.unionWith $ Map.unionWith $ Map.unionWith $ Map.union) t1 t2

instance Monoid BuildTree where
  mempty  = BuildTree $ Map.empty
  mappend = (<>)

-- | A build tree with a single entry.

singleton :: Key -> FilePath -> BuildTree
singleton (Key pkg ver arch major minor) dir = BuildTree $
  Map.singleton pkg $
  Map.singleton ver $
  Map.singleton arch $
  Map.singleton major $
  Map.singleton minor $ Entry dir False

-- | Modify all entries of a build tree.

mapEntry :: (Entry -> Entry) -> BuildTree -> BuildTree
mapEntry f (BuildTree t) = BuildTree $ (fmap . fmap . fmap . fmap . fmap) f t

-- | Modify all entries of a build tree, accumulating effects left-to-right.

traverseEntry :: Applicative m => (Entry -> m Entry) -> BuildTree -> m BuildTree
traverseEntry f (BuildTree t) =
  BuildTree <$> (traverse . traverse . traverse . traverse . traverse) f t

-- | Reduce build tree.

foldMapEntry :: Monoid m => (Entry -> m) -> BuildTree -> m
foldMapEntry f (BuildTree t) = (foldMap . foldMap . foldMap . foldMap . foldMap) f t

-- * Parsing directory names

parseKey :: Arch -> CompilerString -> PackageString -> Maybe Key
parseKey arch hc s = do
  (major, minor) <- parseCompilerString hc
  (pkg  , ver  ) <- parsePackageString s
  return $ Key pkg ver arch major minor

type CompilerString = String

parseCompilerString :: CompilerString -> Maybe CompilerVersion
parseCompilerString s = do
  n <- findIndex (== '-') s
  case splitAt n s of
    ("ghc", _:v) -> splitAt 2 <$> parseVersionString v
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
