module Data.VAS where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Functor ((<$>))

-- * Data types

-- | The base Vector Addition System type.
-- Transitions are stored as a pair of subtractive and additive vectors.
-- These correspond to Pre and Post in most formal definitions.
newtype VAS = VAS ( Vector (V,V) )

instance Show VAS where
    show (VAS v) = unlines $ map show $ (Vector.toList v)

-- | Some type which represents a VAS vector.
-- The implementation details of V may be subject to change.
type V       = Vector Int

-- | When we equip our VAS with a configuration, we say it is configured.
type Configured a = (a, Configuration)

-- | Our configuration type is obviously just a vector.
type Configuration = V
