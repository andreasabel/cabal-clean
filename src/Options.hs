module Options (module Options, module X) where

import System.Console.Pretty ( color, style )
import System.Console.Pretty  as X
  ( Color(Green, Red, White)
  , Style(ColoredNormal, Faint, Italic)
  )
import Util

data Options = Options
  { optDelete     :: Bool
  , optNoColors   :: Bool
  , optVerbose    :: Bool
  , optRoot       :: FilePath
  } deriving Show

-- | Apply ANSI style if coloring is enabled in 'Options'.

styleOpt :: Options -> Style -> String -> String
styleOpt opts = applyWhenColors opts style

colorOpt :: Options -> Color -> String -> String
colorOpt opts = applyWhenColors opts color

applyWhenColors :: Options -> (a -> b -> b) -> a -> b -> b
applyWhenColors opts
  | optNoColors opts = \ _ _ -> id
  | otherwise        = id

-- * Verbosity functionality.

chat :: Options -> String -> IO ()
chat = chatGen $ hPutStr stderr

chatLn :: Options -> String -> IO ()
chatLn = chatGen $ hPutStrLn stderr

chatGen :: (String -> IO ()) -> Options -> String -> IO ()
chatGen prt o msg = when (optVerbose o) $
  prt $ styleOpt o Faint $ unwords ["info:", msg]
