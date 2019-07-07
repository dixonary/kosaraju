module Data.GVASS where

import Data.List ((\\), uncons, intersperse)
import Data.Map  (Map, (!))
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as DV
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

import Prelude hiding (and, zipWith)

import Data.Maybe (fromJust)
import Data.Bifunctor (second)
import Data.Coerce

import Text.Printf

import Data.VASS.Coverability
import Data.VASS
import Data.VASS.Read

data GVASS = GVASS [Component]
    deriving (Show)

type Coordinate     = Integer
type SparseVector a = Map Integer a

-- | A Component is a single element of our GVASS. 
data Component = Component
    { dimension                  :: Integer
    , states                     :: Vector (Name State)
    , transitions                :: Map (Name State) (Vector Transition)
    , initialState               :: Name State
    , finalState                 :: Name State
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


generaliseSpec :: CovProblem -> GVASS
generaliseSpec (CovProblem vass initial target) = generalise vass initial target


-- | Lift a regular VASS so that it can be used in the generalised setting.
-- | IE. take a VASS from "setting 1" to "setting 3" in Sławek's paper.
generalise :: VASS -> Conf -> Conf -> GVASS
generalise VASS{..} (Configuration iS iV) (Configuration fS fV) = GVASS
    [ Component
        { dimension                  = fromIntegral $ length iV
        , states                     = states
        , transitions                = transitions
        , initialState               = iS
        , finalState                 = fS
        , rigidCoords                = Set.empty
        , rigidValues                = Map.empty
        , initialConstrainedCoords   = Set.fromList range
        , initialUnconstrainedCoords = Set.empty
        , finalConstrainedCoords     = Set.fromList range
        , finalUnconstrainedCoords   = Set.empty
        , initialVector              = Map.fromList $ zip range $ DV.toList iV
        , finalVector                = Map.fromList $ zip range $ DV.toList fV
        }
    ]
    where range = [1 .. fromIntegral $ length iV]

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
removeTransition :: Name Transition -> Component -> Component
removeTransition trans c@Component{..} = c
        { transitions = Map.map (DV.filter (\(Transition name _ _ _) -> name /= trans)) transitions
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