{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.VASS.Shared where

import qualified Data.Vector as DV
import Data.Map (Map)
import GHC.Exts (IsString)
import Text.Printf

-- | We use typed labels so that we can leverage type safety on our 
-- states and transitions.
newtype Label a = Label String deriving (Eq, Ord, IsString, PrintfArg)
type Labelled a = (Label a, a)

instance Show (Label a) where show (Label l) = l


-- | A coordinate is an integer.
type Coordinate = Int

type SparseVector a = Map Coordinate a

-- | A configuration is a state paired with a marking.
type Configuration = (Label State, Vector)
type Configured a = (Configuration, a)


-- | A State is identified by name only.
type State = String

-- | A transition (in this model) consists of an *Input* and an *Output* vector.
-- The Input vector is subtracted from the configuration and the Output vector
-- is added to it.
-- A transition can only be fired if it is greater than *input*. 
-- A transition also takes us to a new state, given by the third value.
type Transition = (Vector, Vector, Label State)

pre :: Transition -> Vector
pre (p,_,_) = p

post :: Transition -> Vector
post (_,p,_) = p

nextState :: Transition -> Label State
nextState (_,_,s) = s

-- Sometimes we only need the net values of a transition and no other info.
collapse :: Transition -> Vector
collapse (sub,add,label) = DV.zipWith (-) add sub



-- | A vector V is quite literally just represented as a Vector of Integers.
-- The Integer type is of unbounded size but will behave as a 64-bit integer
-- when small. So this is relatively fast.
type Vector          = DV.Vector Integer


-- | We use the natural partial ordering over vectors of natural numbers.
-- This is defined componentwise, ie a <= b iff every component in a is <= its
-- counterpart in b.
instance {-# OVERLAPS #-} Ord Vector where

    a <= b = DV.and $ DV.zipWith (<=) a b
    a >= b = DV.and $ DV.zipWith (>=) a b

    a <  b = DV.and (DV.zipWith (<=) a b) 
          && DV.or  (DV.zipWith (<)  a b)

    a >  b = DV.and (DV.zipWith (>=) a b) 
          && DV.or  (DV.zipWith (>)  a b)


-- | Arithmetic operations are defined componentwise.
instance {-# OVERLAPS #-} Num a => Num (DV.Vector a) where
    (+) = DV.zipWith (+)
    (-) = DV.zipWith (-)
    (*) = DV.zipWith (*)
    abs         = undefined
    signum      = undefined
    fromInteger = mempty