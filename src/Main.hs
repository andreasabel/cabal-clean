-- | cabal-clean
--
-- Remove build artefacts of outdated versions from `dist-newstyle`.

{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative
  ( execParser, footerDoc, header, help, helper, info, infoOption
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
  { optVerbose    :: Bool
  , optDelete     :: Bool
  } deriving Show

options :: IO Options
options =
  execParser $
    info (helper <*> versionOption <*> numericVersionOption <*> programOptions)
         (header "Clean outdated build artefacts from directory `dist-newstyle`."
          <> footerDoc (Just foot))

  where
  versionOption =
    infoOption (unwords versionWords)
      $  long "version"
      <> help "Show version info."
  versionWords = concat
    [ [ self, "version", version ]
    ]

  numericVersionOption =
    infoOption version
      $  long "numeric-version"
      <> help "Show just version number."
      -- Newline at the end:
      -- <> helpDoc (Just $ text "Show just version number." <$$> text "")

  programOptions = Options
    <$> oVerbose
    <*> oDelete

  oDelete =
    switch
      $  long "delete"
      <> help "Actually clean up on disk."

  oVerbose =
    switch
      $  long "verbose"
      <> short 'v'
      <> help "Comment on what is happening."

  foot = vcat $ map text $ concat
    [ [ "TODO: extended help."
      , ""
      ]
    ]

-- * Verbosity functionality.

chat :: Options -> String -> IO ()
chat = chatGen $ hPutStr stderr

chatLn :: Options -> String -> IO ()
chatLn = chatGen $ hPutStrLn stderr

chatGen :: (String -> IO ()) -> Options -> String -> IO ()
chatGen prt o msg = when (optVerbose o) $
  prt $ style Faint $ unwords ["info:", msg]
