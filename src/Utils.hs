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

-- | Some amount of indentation.
indent :: Int -> String
indent i = concat (replicate i "    ")


class Section a where
    header   :: a -> Maybe String
    doIndent :: a -> Bool
    body     :: a -> [String]

    header   _ = Nothing
    doIndent _ = True

instance Show a => Section [a] where
    body       = \case
            [] -> ["∅"]
            ls -> pure $ concat $ intersperse " " $ map show ls

instance Show a => Section (Set a) where
    body = body . Set.toList

instance (Show k, Show a) => Section (Map k a) where
    body m = case vars of
                []   -> ["∅"]
                ls -> ls

        where vars = [ show k ++ " => " ++ show v | (k, v) <- Map.toList m]

prettyPrint :: Section a => a -> [String]
prettyPrint pp = concat $
    [
        maybeToList $ header pp,
        map ( indent (bool 0 1 (doIndent pp)) ++ ) $ body pp
    ]





-- Helper functions

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



-- Helpful map functions
collate :: Ord a => [(a, b)] -> Map a [b]
collate = foldr (\(k,v) m -> Map.insertWith (++) k [v] m) mempty

decollate :: Ord a => Map a [b] -> [(a,b)]
decollate = concat . Map.mapWithKey (\k vs -> map (k,) vs) 