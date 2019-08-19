{- Use the standard COV->REACH reduction to determine whether some state is coverable.
-}
module Kosaraju.Cov where

import Data.VASS
import Data.VASS.Coverability
import Data.GVASS
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Function ((&))
import Text.Pretty.Simple
import Kosaraju
import Data.Coerce
import Data.Functor (($>))

import Duvet


kosChecker :: CheckerInfo
kosChecker = CheckerInfo
    { checker = kosarajuCover
    , longName  = "kosaraju"
    , shortName = 'j'
    , description = "Solve coverability by reduction to reachability."
    }

kosarajuCover :: CovChecker
kosarajuCover problem = do
    let g = buildCovProblem problem
    pPrint g
    res <- kosaraju g
    return $ case res of
        KosarajuHolds r -> Unsafe
        KosarajuDoesNotHold -> Safe

buildCovProblem :: CovProblem -> GVASS
buildCovProblem problem = 
    -- generaliseSpec gives us a single-component system.
    -- We will want to add a state and subtract our target with a bit extra.
    let (GVASS [c@Component{..}]) = generaliseSpec problem 

        newState = "Δ"
        singleState = head $ Set.toList states

        zeroTransition      = (range dimension :: Vector Coordinate) $> 0
        jumpTransition      = Transition
            { name = "cov_jump"
            , pre  = makeDenseCoords finalVector dimension
            , post = zeroTransition
            , nextState = newState
            }
        windDownTransitions = do
            x <- range dimension :: Vector Coordinate
            return $ Transition
                { name = coerce $ "windDown_" ++ show x
                , pre  = zeroTransition Vector.// [(fromIntegral x, 1)]
                , post = zeroTransition
                , nextState = newState
                }

    in GVASS 
        [ c 
        { states = states <> [newState]
        , finalState = newState
        , finalVector = finalVector $> 0
        , transitions = transitions
            & Map.insertWith (<>) newState windDownTransitions
            & Map.insertWith (<>) singleState [jumpTransition]
        }
        ]