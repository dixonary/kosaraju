{-| A GVASS (Generalised Vector Addition with States) is an extension of the
    standard VASS. In particuler, it embeds an arbitrary number of VASS inside
    of a framework which can provide additional invariants of the system.

    A GVASS comprises a chain of VASSs. The VASSs are annotated with additional
    restrictions, such as the precise values of some coordinates at the entry
    or exit points of the indiviual VASSs (called "components"). Vectors are
    joined together by "adjoinments": singular, known vectors which can modify
    arbitrary vector coordinates.

-}
module Data.GVASS where

import Data.List ((\\), uncons, intersperse)
import Data.Map  (Map, (!))
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
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
import GHC.Exts


{-| A GVASS is a list of components.
    Data.Seq would be a more efficient implementation, but lists do not seem
    to have too negative of an impact (the number of components rarely hits
    double figures).
-}
newtype GVASS = GVASS [Component]
    deriving (Show, Eq)

-- | Extract the linked list of components from the GVASS.
unGVASS :: GVASS -> [Component]
unGVASS (GVASS cs) = cs

-- | We define a coordinate simply as an integer.
type Coordinate     = Integer

-- | A sparse vector is a mapping from coordinate to value.
type SparseVector a = Map Coordinate a


-- | A Component is a single element of our GVASS. 
data Component = Component
    { dimension                  :: Integer
    , states                     :: Set (Name State)
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
    , adjoinment                 :: Maybe (Vector Integer)
    }
    deriving (Show, Eq)

{-| Convert a coverability problem into a GVASS problem.
    This doesn't actually provide any new insights, but it allows us to
    abuse the existing specification formats to provide reachability problems
    as well as coverability problems.

    Note that this makes the target go from the standard coverability metric
    to the standard reachability metric! To perform coverability with this
    tool, see 'Data.Kosaraju.Cov.kosChecker'.
-}
generaliseSpec :: CovProblem -> GVASS
generaliseSpec (CovProblem vass initial target) = generalise vass initial target


{-| Lift a regular VASS so that it can be used in the generalised setting.
    IE. take a VASS from "setting 1" to "setting 3" in Sławek's note.
-}
generalise :: VASS -> Conf -> Conf -> GVASS
generalise VASS{..} (Configuration iS iV) (Configuration fS fV) = GVASS
    [ Component
        { dimension                  = dimension
        , states                     = states
        , transitions                = transitions
        , initialState               = iS
        , finalState                 = fS
        , rigidCoords                = Set.empty
        , rigidValues                = Map.empty
        , initialConstrainedCoords   = allCoords dimension
        , initialUnconstrainedCoords = Set.empty
        , finalConstrainedCoords     = allCoords dimension
        , finalUnconstrainedCoords   = Set.empty
        , initialVector              = Map.fromList $ zip (range dimension) $ Vector.toList iV
        , finalVector                = Map.fromList $ zip (range dimension) $ Vector.toList fV
        , adjoinment                 = Nothing
        }
    ]
    where dimension = fromIntegral $ length iV

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


-- | Set the initial constrained coordinate set to a specific vector.
setInitial :: SparseVector Integer -> Component -> Component
setInitial vec c@Component{..} = c
        { initialConstrainedCoords   = coords
        , initialUnconstrainedCoords = allCoords dimension Set.\\ coords
        , initialVector              = vec
        }
        where coords = Set.fromList $ Map.keys vec

-- | Set the final constrained coordinate set to a specific vector.
setFinal :: SparseVector Integer -> Component -> Component
setFinal vec c@Component{..} = c
        { finalConstrainedCoords   = coords
        , finalUnconstrainedCoords = allCoords dimension Set.\\ coords
        , finalVector              = vec
        }
        where coords = Set.fromList $ Map.keys vec

-- | Set the delta between one component and the next to a fixed value.
setAdjoinment :: Maybe (Vector Integer) -> Component -> Component
setAdjoinment vec c@Component{..} = c { adjoinment = vec }


-- | Fully remove a transition from some component.
removeTransition :: Name Transition -> Component -> Component
removeTransition trans c@Component{..} = c
        { transitions = Map.map (Vector.filter (\(Transition name _ _ _) -> name /= trans)) transitions
        }

-- | Remove ALL constraints over values.
unconstrainAll :: Component -> Component
unconstrainAll c@Component{..} = c
        { initialConstrainedCoords   = Set.empty
        , initialUnconstrainedCoords = allCoords dimension
        , finalConstrainedCoords     = Set.empty 
        , finalUnconstrainedCoords   = allCoords dimension
        , initialVector              = Map.empty
        , finalVector                = Map.empty
        }


instance {-# OVERLAPS #-} Show a => Show (Map a Integer) where
    show map = concat
         $  ["{"]
         ++ intersperse "," [ show a ++ " => " ++ show b | (a,b) <- Map.toList map ]
         ++ ["}"]

instance {-# OVERLAPS #-} Show (Set Integer) where
    show set = case length set of
        0 -> "empty"
        _ ->
            "{"
         ++ unwords (show <$> Set.toList set)
         ++ "}"



--------------------------------------------------------------------------------
-- * Helpers

{-| We can measure the meaningful size of a system by the number of components 
    it has.
-}
class TotalComponents a where
    totalComponents :: a -> Int

instance TotalComponents Component where 
    totalComponents _                  = 1
instance TotalComponents GVASS     where 
    totalComponents (GVASS components) = length components
instance TotalComponents [GVASS]   where 
    totalComponents                    = sum . map totalComponents

{-| Highly polymorphic: get all elements in a range, like the range() function
    in Python.
-}
range :: (Integral a, IsList (f a), Item (f a) ~ a) => a -> f a
range x = [0 .. x-1]

-- | @allCoords@ is just a monomorphic wrapper around 'range'.
allCoords :: Integer -> Set Coordinate
allCoords = range

-- | Get only the (dense) delta resulting from the application of the transition.
flatten :: Transition -> Vector Integer
flatten (Transition name pre post nextState) = Vector.zipWith (-) post pre

-- | Convert a dense vector into a sparse one, where zeroes will be omitted.
makeSparseCoords :: Vector Integer -> SparseVector Coordinate
makeSparseCoords vs = Map.fromList 
        [ (fromIntegral i, j)
        | (i,j) <- Vector.toList $ Vector.indexed vs
        , j /= 0
        ]

{-| Convert a sparse vector into a dense one, where zeroes are inserted in
    unknown places.

    We have to provide a dimension component so that we can fill zeroes
    right to the end of the vector.
-}
makeDenseCoords :: SparseVector Integer -> Integer -> Vector Integer
makeDenseCoords sparse dimension 
    = fmap (\p -> Map.findWithDefault 0 p sparse) (range dimension)