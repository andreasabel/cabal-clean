-- | cabal-clean
--
-- Remove build artefacts of outdated versions from `dist-newstyle`.

{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative
  ( Parser
  , action, execParser, footerDoc, headerDoc, help, helper, hidden
  , info, infoOption, long, metavar, short, strArgument, switch, value
  )
import Options.Applicative.Help.Pretty
  ( vcat, text )

import System.Console.Pretty
  ( supportsPretty )
import System.IO
  ( hPutStr, hPutStrLn, stderr )

import License
  ( license )
import Options
import Structure
  ( Entry(..), foldMapEntry, getBuildTree, markObsolete, printBuildTree )
import Util
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

  -- Get build tree.

  chatLn opts $ unwords [ "Reading", buildDir, "..." ]
  unlessM (doesDirectoryExist buildDir) $ do
    die $ unwords [ "No such directory:", buildDir ]

  (tree0, warns) <- getBuildTree buildDir
  forM_ warns $ putStrLn . ("warning: " ++)

  let tree = markObsolete tree0
  printBuildTree opts tree

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

  licenseOption =
    infoOption license
      $  long "license"
      <> long "licence"
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
    , unwords
      [ "A package build is considered superseded if there is a local build"
      , "of either a newer version of the package or with a newer minor version"
      , "of the Haskell compiler."
      ]
    ]
  footer = Just $ vcat $ map (text . unwords)
    [ [ unwords ["Without option --delete,", self, "does not actually clean out anything,"]
      , "just shows prefixed with '---' and in red what would be removed and prefixed with '+++' and in green what is kept."
      ]
    , [ "" ]
    , [ "Warning: there is check whether the to-be-deleted contents are actually garbage."
      , "(E.g., there could be symlinks to executables stored there.)"
      ]
    ]

-- * Verbosity functionality.

chat :: Options -> String -> IO ()
chat = chatGen $ hPutStr stderr

chatLn :: Options -> String -> IO ()
chatLn = chatGen $ hPutStrLn stderr

chatGen :: (String -> IO ()) -> Options -> String -> IO ()
chatGen prt o msg = when (optVerbose o) $
  prt $ styleOpt o Faint $ unwords ["info:", msg]
