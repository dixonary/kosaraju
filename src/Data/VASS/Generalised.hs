module Data.VASS.Generalised where

import Data.List ((\\), uncons, intersperse)
import Data.Map  (Map, (!))
import qualified Data.Map as Map
import qualified Data.Vector as DV
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

import Data.VASS.Shared
import Data.VASS (VASS(VASS)) 
import qualified Data.VASS as VASS

import Prelude hiding (and, zipWith)

import Data.Maybe (fromJust)
import Data.Bifunctor (second)
import Data.Coerce

import Text.Printf

import Data.VAS (VAS)
import Data.VAS.Read (Spec(..))

data GVASS = GVASS [Component]
    deriving (Show)



-- | A Component is a single element of our GVASS. 
data Component = Component
    { dimension                  :: Int
    , states                     :: [Label State]
    , transitions                :: Map (Label State) [Labelled Transition]
    , initialState               :: Label State
    , finalState                 :: Label State
    , rigidCoords                :: Set Coordinate
    , rigidValues                :: SparseVector Integer
    , initialConstrainedCoords   :: Set Coordinate
    , initialUnconstrainedCoords :: Set Coordinate
    , finalConstrainedCoords     :: Set Coordinate
    , finalUnconstrainedCoords   :: Set Coordinate
    , initialVector              :: SparseVector Integer
    , finalVector                :: SparseVector Integer
    }
    deriving (Show)


generaliseSpec :: Spec -> GVASS
generaliseSpec (Spec vas initial target) = let
    vass = VASS.fromVAS vas
    muState = head $ VASS.states vass
    in generalise vass 
        (muState, fromIntegral <$> initial) 
        (muState, fromIntegral <$> target )


-- | Lift a regular VASS so that it can be used in the generalised setting.
-- | IE. take a VASS from "setting 1" to "setting 3" in Sławek's paper.
generalise :: VASS -> Configuration -> Configuration -> GVASS
generalise VASS{..} (iS, iV) (fS, fV) = GVASS
    [ Component
        { dimension                  = length iV
        , states                     = states
        , transitions                = transitions
        , initialState               = iS
        , finalState                 = fS
        , rigidCoords                = Set.empty
        , rigidValues                = Map.empty
        , initialConstrainedCoords   = Set.fromList [1 .. length iV]
        , initialUnconstrainedCoords = Set.empty
        , finalConstrainedCoords     = Set.fromList [1 .. length iV]
        , finalUnconstrainedCoords   = Set.empty
        , initialVector              = Map.fromList $ zip [1..length iV] $ DV.toList iV
        , finalVector                = Map.fromList $ zip [1..length iV] $ DV.toList fV
        }
    ]

-- | Replace some component in the VASS with a modified version, as specified by @modFunc.
modifyComponent :: GVASS -> Int -> (Component -> Component) -> GVASS
modifyComponent (GVASS components) componentIndex modFunc =
    GVASS components'
    where
        components' = prefix ++ [c'] ++ suffix
        (prefix, (c, suffix)) = second (fromJust . uncons) $ splitAt componentIndex components
        c' = modFunc c

-- | Replace some component with a sequence of components, as specified by @expandFunc.
expandComponent :: GVASS -> Int -> (Component -> [Component]) -> GVASS
expandComponent (GVASS components) componentIndex expandFunc = 
    GVASS components'
    where
        components' = prefix ++ cs ++ suffix
        (prefix, (c, suffix)) = second (fromJust . uncons) $ splitAt componentIndex components
        cs = expandFunc c


-- | Set some initial place to a specific value.
constrainInitial :: Coordinate -> Integer -> Component -> Component
constrainInitial coord constrainedValue c@Component{..} = c
        { initialUnconstrainedCoords = Set.delete coord initialUnconstrainedCoords 
        , initialConstrainedCoords   = Set.insert coord initialConstrainedCoords
        , initialVector              = Map.insert coord constrainedValue initialVector
        }

-- | Set some final place to a specific value.
constrainFinal :: Coordinate -> Integer -> Component -> Component
constrainFinal coord constrainedValue c@Component{..} = c   
        { finalUnconstrainedCoords = Set.delete coord finalUnconstrainedCoords 
        , finalConstrainedCoords   = Set.insert coord finalConstrainedCoords
        , finalVector              = Map.insert coord constrainedValue finalVector
        }


-- | Remove a specific value from some initial place.
unconstrainInitial :: Coordinate -> Component -> Component
unconstrainInitial coord c@Component{..} = c 
        { initialConstrainedCoords   = Set.delete coord initialConstrainedCoords 
        , initialUnconstrainedCoords = Set.insert coord initialUnconstrainedCoords
        , initialVector              = Map.delete coord initialVector
        }

-- | Remove a specific value from some final place.
unconstrainFinal :: Coordinate -> Component -> Component
unconstrainFinal coord c@Component{..} = c 
        { finalConstrainedCoords   = Set.delete coord finalConstrainedCoords 
        , finalUnconstrainedCoords = Set.insert coord finalUnconstrainedCoords
        , finalVector              = Map.delete coord finalVector
        }


-- Set the initial constrained coordinate set to a specific vector.
setInitial :: SparseVector Integer -> Component -> Component
setInitial vec c@Component{..} = c
        { initialConstrainedCoords   = coords
        , initialUnconstrainedCoords = Set.fromList [1..dimension] Set.\\ coords
        , initialVector              = vec
        }
        where coords = Set.fromList $ Map.keys vec

-- Set the final constrained coordinate set to a specific vector.
setFinal :: SparseVector Integer -> Component -> Component
setFinal vec c@Component{..} = c
        { finalConstrainedCoords   = coords
        , finalUnconstrainedCoords = Set.fromList [1..dimension] Set.\\ coords
        , finalVector              = vec
        }
        where coords = Set.fromList $ Map.keys vec


-- | Fully remove a transition from some component.
removeTransition :: Label Transition -> Component -> Component
removeTransition trans c@Component{..} = c
        { transitions = Map.map (filter (\t -> fst t /= trans)) transitions
        }

-- | Remove ALL constraints over values.
unconstrainAll :: Component -> Component
unconstrainAll c@Component{..} = c
        { initialConstrainedCoords   = Set.empty
        , initialUnconstrainedCoords = Set.fromList [1..dimension]
        , finalConstrainedCoords     = Set.empty 
        , finalUnconstrainedCoords   = Set.fromList [1..dimension]
        , initialVector              = Map.empty
        , finalVector                = Map.empty
        }



-- * Helpers

-- We can measure the meaningful size of a system by the number of components it has.

class TotalComponents a where
    totalComponents :: a -> Int

instance TotalComponents Component where 
    totalComponents _                  = 1
instance TotalComponents GVASS     where 
    totalComponents (GVASS components) = length components
instance TotalComponents [GVASS]   where 
    totalComponents                    = sum . map totalComponents