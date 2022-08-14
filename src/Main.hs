-- | cabal-clean
--
-- Remove build artefacts of outdated versions from `dist-newstyle`.

{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Monoid
  ( Sum(..) )

import Options.Applicative
  ( Parser
  , action, execParser, footerDoc, headerDoc, help, helper, hidden
  , info, infoOption, long, metavar, short, strArgument, switch, value
  )
import Options.Applicative.Help.Pretty
  ( vcat, text )

import System.Console.Pretty
  ( supportsPretty )

import Util

import DiscoverGHCs
  ( discoverGHCs )
import License
  ( copyright, license )
import Options
import Structure
  ( Entry(..), foldMapEntry, getBuildTree, markObsolete, printBuildTree )
import Version

self :: String
self = "cabal-clean"

homepage :: String
homepage = concat [ "https://github.com/andreasabel/", self ]

defaultRoot :: FilePath
defaultRoot = "dist-newstyle"

main :: IO ()
main = do

  -- Parse options.

  opts@Options{..} <- options
  -- print opts
  let buildDir = optRoot </> "build"

  -- Discover installed GHCs.
  ghcs <- discoverGHCs opts

  -- Get build tree.

  chatLn opts $ unwords [ "Reading", buildDir, "..." ]
  unlessM (doesDirectoryExist buildDir) $ do
    die $ unwords [ "No such directory:", buildDir ]

  (tree0, warns) <- getBuildTree buildDir
  forM_ warns $ putStrLn . ("warning: " ++)

  -- Mark superseded builds as obsolete.
  -- If no installed GHCs could be found, assume a misconfiguration and
  -- do not use installed GHCs as criterion.
  let keep = if null ghcs then const True else hasElem ghcs
  let tree = markObsolete keep tree0
  printBuildTree opts tree

  -- Count obsolete directories.
  let nObs :: Integer
      nObs = getSum $ foldMapEntry ((\ x -> if x then Sum 1 else Sum 0) . obsolete) tree

  if nObs <= 0 then putStrLn ("Nothing to clean!")
  else if not optDelete then putStrLn $ unwords
    [ show nObs, "directories can be removed (supply option --remove)." ]
  -- Remove obsolete directories.
  else do
    putStrLn $ unwords [ "Removing", show nObs, "obsolete directories..." ]
    flip foldMapEntry tree $ \ (Entry dir obsolete) -> do
      if obsolete then do
        chatLn opts $ unwords [ "Removing", dir ]
        removeDirectoryRecursive dir
      else chatLn opts $ unwords [ "Keeping", dir ]

  -- Done.

  chatLn opts $ unwords [ self, "terminated successfully." ]

-- * Option parsing and handling

options :: IO Options
options = do
  opts <- execParser $ info parser infoMod
  supportsPretty >>= \case
    True  -> return opts
    False -> return opts{ optNoColors = True }
  where
  parser = programOptions <**>
    (versionOption <*> numericVersionOption <*> licenseOption <*> helper)
  infoMod = headerDoc header <> footerDoc footer

  versionOption =
    infoOption versionLong
      $  long "version"
      <> short 'V'
      <> help "Show version info."
      <> hidden
  versionText = unwords [ self, "version", version ]
  versionLong = intercalate "\n" $
    [ versionText
    , copyright
    , "This is free software under the BSD-3-clause license."
    ]

  numericVersionOption =
    infoOption version
      $  long "numeric-version"
      <> help "Show just version number."
      <> hidden
      -- Newline at the end:
      -- <> helpDoc (Just $ text "Show just version number." <$$> text "")

  licenseOption =
    infoOption license
      $  long "license"
      -- <> long "licence"
      <> help "Show the license text."
      <> hidden

  -- Obs: match the order with Options.Options!
  programOptions = Options
    <$> oDelete
    <*> oNoColors
    <*> oVerbose
    <*> oRoot

  oDelete =
    switch
      $  long "delete"
      <> long "remove"
      -- <> short 'D'
      <> help "Actually clean up on disk."

  oVerbose =
    switch
      $  long "verbose"
      <> short 'v'
      <> help "Comment on what is happening."

  oNoColors =
    switch
      $  long "no-colors"
      <> help "Disable colorized output.  Automatic if terminal does not support colors."

  oRoot :: Parser FilePath
  oRoot = do
    strArgument
      $  metavar "ROOT"
      <> value defaultRoot
      <> action "directory"
      <> help (concat ["The root of the build tree. Default: ", show defaultRoot, "."])

  -- Note: @header@ does not respect line breaks, so we need @headerDoc@.
  header = Just $ vcat $ map text
    [ unwords [ versionText, homepage ]
    , ""
    , concat
      [ "Clean superseded build artefacts from directory ROOT (typically "
      , show defaultRoot
      , ")."
      ]
    , ""
    , "A package build is considered superseded if one of the following conditions is met:"
    , "- It was build with a Haskell compiler that cannot be found on the system PATH."
    , "- There is a build of a newer version of the package."
    , "- There is a build with a newer minor version of the Haskell compiler."
    , ""
    , "Limitation: Only GHC is recognized as Haskell compiler, and only in the form 'ghc-VERSION' (not just 'ghc')."
    ]
  footer = Just $ vcat $ map (text . unwords)
    [ [ unwords ["Without option --delete,", self, "does not actually clean out anything,"]
      , "just shows prefixed with '---' and in red what would be removed and prefixed with '+++' and in green what is kept."
      ]
    , [ "" ]
    , [ "Warning: there is no check whether the to-be-deleted contents are actually garbage."
      , "(E.g., there could be symlinks to executables stored there.)"
      ]
    ]
