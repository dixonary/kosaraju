module Utils where

import Data.Maybe (maybeToList)
import Data.List (concat, intersperse)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bool (bool)
import qualified Data.Vector as DV

import Data.Function ((&))
import Data.Bifunctor (Bifunctor, first, second)
import Data.Monoid

--------------------------------------------------------------------------------
-- * Helper functions

{-# INLINE toMaybe #-}
toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x

project :: Eq a => Set Int -> DV.Vector a -> DV.Vector a
project set vec = catMaybes 
        $ DV.map (\(i,s) -> toMaybe ((i+1) `elem` set) s) 
        $ DV.indexed vec 


catMaybes :: (Applicative f, Foldable f, Monoid (f a)) => f (Maybe a) -> f a
catMaybes = mconcat . foldr poss []
        where 
            poss :: Applicative f => Maybe a -> [f a] -> [f a]
            poss (Just x) s = pure x : s
            poss Nothing  s = s

