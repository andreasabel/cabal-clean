-- | cabal-clean
--
-- Remove build artefacts of outdated versions from `dist-newstyle`.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import qualified Data.List as List
import Data.Semigroup

import Options.Applicative
import Options.Applicative.Help.Pretty (vcat, text)

import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath
  (splitExtension, addExtension, takeFileName, takeDirectory, hasTrailingPathSeparator, (</>))
import System.IO        (hPutStr, stderr)
import System.Exit      (exitFailure)

import Structure        (getBuildTree, markObsolete, printBuildTree)
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

  unlessM (doesDirectoryExist buildDir) $ do
    die $ unwords [ "No such directory:", buildDir ]

  (tree0, warns) <- getBuildTree buildDir
  forM_ warns $ putStrLn . ("warning: " ++)

  let tree = markObsolete tree0
  printBuildTree tree

  -- Done.

  chat opts $ unwords [ self, "terminated successfully.\n" ]

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

data DirOrFile = Dir FilePath | File FilePath

dirOrFile :: FilePath -> IO DirOrFile
dirOrFile path
  | hasTrailingPathSeparator path = return $ Dir path
  | otherwise = doesDirectoryExist path <&> \case
      True  -> Dir path
      False -> File path

chat :: Options -> String -> IO ()
chat o msg = when (optVerbose o) $ hPutStr stderr msg
