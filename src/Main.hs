-- | cabal-clean
--
-- Remove build artefacts of outdated versions from `dist-newstyle`.

{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative
  ( execParser, footerDoc, headerDoc, help, helper, hidden, info, infoOption
  , long, short, switch )
import Options.Applicative.Help.Pretty
  ( vcat, text )

import System.IO
  ( hPutStr, hPutStrLn, stderr )

import Structure
  ( Entry(..), foldMapEntry, getBuildTree, markObsolete, printBuildTree )
import Util
import Version

self :: String
self = "cabal-clean"

homepage :: String
homepage = concat [ "https://github.com/andreasabel/", self ]

buildDir :: FilePath
buildDir = "dist-newstyle" </> "build"

main :: IO ()
main = do

  -- Parse options.

  opts@Options{..} <- options

  -- Get build tree.

  chatLn opts $ unwords [ "Reading", buildDir, "..." ]
  unlessM (doesDirectoryExist buildDir) $ do
    die $ unwords [ "No such directory:", buildDir ]

  (tree0, warns) <- getBuildTree buildDir
  forM_ warns $ putStrLn . ("warning: " ++)

  let tree = markObsolete tree0
  printBuildTree tree

  -- Remove obsolete directories
  when optDelete $ do
    chatLn opts $ unwords [ "Removing obsolete directories..." ]
    flip foldMapEntry tree $ \ (Entry dir obsolete) -> do
      if obsolete then do
        chatLn opts $ unwords [ "Removing", dir ]
        removeDirectoryRecursive dir
      else chatLn opts $ unwords [ "Keeping", dir ]

  -- Done.

  chatLn opts $ unwords [ self, "terminated successfully." ]

-- * Option parsing and handling

data Options = Options
  { optDelete     :: Bool
  , optVerbose    :: Bool
  } deriving Show

options :: IO Options
options = execParser $ info parser infoMod
  where
  parser = programOptions <**> (versionOption <*> numericVersionOption <*> helper)
  infoMod = headerDoc header <> footerDoc footer

  versionOption =
    infoOption versionText
      $  long "version"
      <> help "Show version info."
      <> hidden
  versionText = unwords [ self, "version", version ]

  numericVersionOption =
    infoOption version
      $  long "numeric-version"
      <> help "Show just version number."
      <> hidden
      -- Newline at the end:
      -- <> helpDoc (Just $ text "Show just version number." <$$> text "")

  programOptions = Options
    <$> oDelete
    <*> oVerbose

  oDelete =
    switch
      $  long "delete"
      <> help "Actually clean up on disk."

  oVerbose =
    switch
      $  long "verbose"
      <> short 'v'
      <> help "Comment on what is happening."

  -- Note: @header@ does not respect line breaks, so we need @headerDoc@.
  header = Just $ vcat $ map text
    [ unwords [ versionText, homepage ]
    , ""
    , "Clean superseded build artefacts from directory `dist-newstyle`."
    , unwords
      [ "A package build is considered superseded if there is a local build"
      , "of either a newer version of the package or with a newer minor version"
      , "of the Haskell compiler."
      ]
    ]
  footer = Just $ vcat $ map text
    [ unwords ["Without option --delete,", self, "does not actually clean out anything,"]
    , "just shows in red what would be removed and in green what is kept."
    , "You need a terminal with ANSI colors to read this properly."
    , ""
    , "Warning: there is check whether the to-be-deleted contents are actually garbage."
    , "(E.g., there could be symlinks to executables stored there.)"
    ]

-- * Verbosity functionality.

chat :: Options -> String -> IO ()
chat = chatGen $ hPutStr stderr

chatLn :: Options -> String -> IO ()
chatLn = chatGen $ hPutStrLn stderr

chatGen :: (String -> IO ()) -> Options -> String -> IO ()
chatGen prt o msg = when (optVerbose o) $
  prt $ style Faint $ unwords ["info:", msg]
