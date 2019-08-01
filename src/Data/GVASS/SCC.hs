module Data.GVASS.SCC where

import Data.VASS
import Data.GVASS

import Data.Map.Strict as Map       (Map, (!))
import Data.Vector     as Vector    (Vector)

import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import qualified Data.Vector     as Vector
import qualified Data.Set        as Set

import Data.Graph
import qualified Data.Text.Lazy as Text


import qualified Control.Monad.State as MTL
import Control.Monad (foldM, forM_, void)

import Data.Bifunctor (second)
import Data.Tuple (swap)

import Debug.Trace
import System.IO.Unsafe
import Text.Pretty.Simple

{- | Perform the Strongly Connected Component Decomposition.

    If some component is not a SCC, then we refine it by replacing the component
    with every possible route through the strongly connected components within
    it.

    This must be performed for every non-SC component in the GVASS, and so
    there is a good chance of exponential blowup.
-}

decompGVASS :: GVASS -> [GVASS]
decompGVASS gvass@(GVASS components) = do

    -- Decompose each component individually
    let newComps = Vector.toList $ second decompComponent 
               <$> Vector.indexed (Vector.fromList components)

    -- Create the "cross product" modified GVASS
    ($ gvass) . foldr1 (.) 
        <$> sequence [ [ \g -> expandComponent g i (const n) | n <- ns] 
                     | (i,ns) <- newComps
                     ]


-- | Go from one component to a series of components
-- representing the SCC decomposition of that component.
decompComponent :: Component -> [[Component]]
decompComponent comp@Component{..} =  

    -- Construct a Graph representation of the transition system.
    let adjacencies = [ (q, q, Vector.toList $ nextState <$> ts) 
                      | q  <- Vector.toList states
                      , let ts = transitions !@ q
                      ]

    -- Find the strongly connected subgraphs
        sccs :: [(Integer, SCC (Name State))]
        sccs = zip [0..] $ stronglyConnComp adjacencies

    -- Map sccs to a list of states.
        sccMap :: Map Integer [Name State]
        sccMap = Map.fromList $ second flattenSCC <$> sccs

    -- Map states to some SCC.
        stateMap :: Map (Name State) Integer
        stateMap = Map.fromList $ fmap swap 
                 $ ungroup $ second flattenSCC <$> sccs

    -- For each SCC, get the outbound states and how they join to other states.
        sccAdjMap :: Map Integer [(Name State, Integer, Name State, Vector Integer)]
        sccAdjMap = collate $ List.nub 
            [ (i, (q, i', q', flatten trans))
            | (i, scc) <- sccs             -- Get some SCC
            , q        <- flattenSCC scc   -- Get all member states
            , trans    <- Vector.toList $ transitions !@ q  -- Find adjacent
            , let q' = nextState trans
            , let i' = stateMap  ! q'      -- Find its member SCC
            , i' /= i                      -- Remove reflexivity
            ]


    -- Each SCC, ie a subset of states of the component, become their own
    -- component. The bridges between SCCS will be represented as
    -- adjoinments between components.
    -- Each route from start to end is written a sequence of components.
        makeBaseComponent :: [Name State] -> Component
        makeBaseComponent states = comp 
                { states                     = Vector.fromList states
                , transitions                = limitTransitions transitions
                , rigidCoords                = rigidCoords
                , rigidValues                = rigidValues
                , initialConstrainedCoords   = mempty
                , initialUnconstrainedCoords = Set.fromList [1..dimension]
                , finalConstrainedCoords     = mempty
                , finalUnconstrainedCoords   = Set.fromList [1..dimension]
                , initialVector              = mempty
                , finalVector                = mempty
                , adjoinment                 = Nothing
                }
                where limitTransitions ts 
                        =   Vector.filter ((`elem` states) . nextState)
                        <$> Map.fromList [(s, ts !@ s) | s <- states]

    -- The components before anything has been modified about them.
        sccBaseComponents :: Map Integer Component
        sccBaseComponents = Map.fromList 
            [ (i, makeBaseComponent $ sccMap ! i ) | (i,_) <- sccs ]


    -- TODO: Recode findRoutes as generateComponentRoutes
    -- (basically the same, but set the adjoinments to Nothing in between each step)
    -- and set the initial/final states!

    -- Also instead of general adjacencies between SCCs we have to consider EVERY bridge between SCCs.

        initialSCC = stateMap ! initialState
        finalSCC   = stateMap ! finalState

        generateComponentRoutes :: Integer -> Name State -> [Component] -> MTL.State [[Component]] ()
        generateComponentRoutes scc initState ancestorComponents = do
            let comp      = (sccBaseComponents ! scc) { initialState = initState }
                outgoings = sccAdjMap ! scc
            if scc == finalSCC
                then
                    MTL.modify  (List.reverse (comp:ancestorComponents) :)
                else
                    forM_ outgoings $ \(finState, nextSCC, nextInit, adj) -> do
                        let comp' = comp 
                                { finalState = finState
                                , adjoinment = Just $ makeSparseCoords adj 
                                }
                        generateComponentRoutes nextSCC nextInit (comp' : ancestorComponents)
        
        compRoutes :: [[Component]]
        compRoutes = MTL.execState (generateComponentRoutes initialSCC initialState []) []

        -- Give the first component in the list the same (initial) properties as the 
        -- original component.
        makeFirstComp :: GVASS -> GVASS
        makeFirstComp g = modifyComponent g 0 makeFirst
                where makeFirst c = c
                        { initialConstrainedCoords   = initialConstrainedCoords
                        , initialUnconstrainedCoords = initialUnconstrainedCoords
                        , initialVector              = initialVector
                        }

        -- Give the last component in the list the same (final) properties as the
        -- original component.
        makeLastComp :: GVASS -> GVASS
        makeLastComp g = modifyComponent g (totalComponents g - 1) makeLast
                where makeLast c = c
                        { finalConstrainedCoords   = finalConstrainedCoords
                        , finalUnconstrainedCoords = finalUnconstrainedCoords
                        , finalVector              = finalVector
                        , adjoinment               = adjoinment
                        }

        generatedGVASSs = makeFirstComp . makeLastComp . GVASS <$> compRoutes

    in unGVASS <$> generatedGVASSs




--------------------------------------------------------------------------------
-- Helper functions

-- Make association lists into lists of pairs.
ungroup :: (Show a, Show b) => [(a,[b])] -> [(a,b)]
ungroup xs = [ (a,b) | (a,bs) <- xs, b <- bs ]

-- Collect such pairs into a map (kinda the opposite)
collate :: (Ord a, Applicative f, Semigroup (f b)) => [(a,b)] -> Map a (f b)
collate xs = Map.fromListWith (<>) $ second pure <$> xs

(!@) :: (Ord k, Monoid (m a)) => Map k (m a) -> k -> (m a)
map !@ elem = Map.findWithDefault mempty elem map

