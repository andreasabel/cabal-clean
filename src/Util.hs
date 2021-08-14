-- | General-purpose utilities and standard imports.

module Util (module Util, module X) where

import Control.Monad  as X ((<=<), filterM, forM, guard, unless)
import Control.Monad.IO.Class as X (MonadIO, liftIO)
import Control.Monad.Writer as X (WriterT, runWriterT, tell)

import Data.Bifunctor as X
import Data.Char      as X (isSpace)
import Data.Function  as X (on)
import Data.List      as X (findIndex, findIndices)
import Data.List.Split as X (splitWhen)
import Data.Maybe     as X
import Data.Map       as X (Map)
import Data.Version   as X (Version(..))

import System.Directory as X (doesDirectoryExist, listDirectory)
import System.Exit      as X (die)
import System.FilePath  as X ((</>))

import Text.Read      as X (readMaybe)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

forMM :: (Monad m, Monoid b) => m [a] -> (a -> m b) -> m b
forMM m k = mconcat <$> do mapM k =<< m

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM cond m = cond >>= (`unless` m)

findIndexEnd :: Eq a => (a -> Bool) -> [a] -> Maybe Int
findIndexEnd p = lastMaybe . findIndices p

lastMaybe :: [a] -> Maybe a
lastMaybe []     = Nothing
lastMaybe (a:as) = Just $ last1 a as

last1 :: a -> [a] -> a
last1 a []     = a
last1 _ (a:as) = last1 a as
