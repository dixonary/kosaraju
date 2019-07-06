module Data.VASS where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Data.Bifunctor (second)

import Data.VAS hiding (Configuration, Configured)

import Data.VASS.Shared
import Text.Printf
import Data.Coerce

import Utils

-- | Vector Addition Systems with States (VASS) are a common formalism used
-- to represent programs, business processes, and much more.
data VASS = VASS 
    { states      :: [Label State]
    , transitions :: Map (Label State) [Labelled Transition]
    }


instance Show VASS where
    show VASS{..} = 
        let
            showStates = unwords $ map show states
            showState q = show q ++ ":\n" ++ unlines (map showTrans (transitions ! q))
            showTrans (n, (a, b, c)) = printf "  %s:  %s  %s  %s"
                                       n (show a) (show b) (show c)
        in 
            "States:\n" ++ showStates ++ "\n\nTransitions:\n" ++ unlines (map showState states)

            
-- | Get the transitions in the VASS which are active from a given configuration.
activeTransitions :: VASS -> Configuration -> [Labelled Transition]
activeTransitions VASS{..} (q, m) = filter (\(_, (a,_,_)) -> a >= m) $ transitions ! q

fromConfiguredVAS :: Configured VAS -> Configured VASS
fromConfiguredVAS = second fromVAS

fromVAS :: VAS -> VASS
fromVAS (VAS ts) = let
    states = ["μ"]
    makeTrans :: Int -> (V,V) -> Labelled Transition
    makeTrans i (pre, post) = (,)
                              (coerce $ "t_"++show i)
                              (fromIntegral <$> pre, fromIntegral <$> post, "μ")

    t = zipWith makeTrans [1..] (Vector.toList ts)
    transitions = Map.fromList [("μ", t)]
    in VASS{..}

reverse :: VASS -> VASS
reverse s@VASS{..} = s{
        transitions = collate $ map swap $ decollate transitions
    }
    where 
        swap :: (Label State, Labelled Transition) -> (Label State, Labelled Transition)
        swap (preState, (label, (pre,post,postState) ) ) 
                = (postState, (coerce $ coerce label ++ "'", (post, pre, preState) ) )