{-| 
Module: Kosaraju
An implementation of Kosaraju's famous algorithm which implements
the Reachability decision problem over Vector Addition Systems with States.
-}

module Kosaraju where

import Data.SBV
import Documentation.SBV.Examples.Existentials.Diophantine (ldn, basis, Solution(..))

import Data.List ((\\), elemIndex, find, transpose, concat, concatMap)
import Data.Map.Strict (Map)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.GVASS
import Data.GVASS.SCC
import Data.VASS.Coverability.KarpMiller.ExtendedNaturals
import Data.VASS.Coverability.KarpMiller.Shared
import Data.VASS.Coverability.KarpMiller
import Data.VASS

import Data.Functor ((<&>))
import Data.Bifunctor (first, second)
import Data.Function ((&), on)
import Control.Monad (when, forM)
import Data.Foldable (foldrM, maximumBy)
import Data.Ord (comparing)
import Data.Maybe hiding (catMaybes)

import Data.Tree (Tree(..))
import qualified Data.Tree as Tree

import Control.Monad.Trans as MTL
import qualified Control.Monad.State as MTL
import Control.Monad.Except

import System.IO.Unsafe

import Prelude hiding (reverse)

import Data.Coerce
import Text.Pretty.Simple
import Debug.Trace
import Text.Printf


--------------------------------------------------------------------------------
-- * Kosaraju's Algorithm

kosaraju :: GVASS -> IO KosarajuResult
kosaraju g = kosaraju' [g]

{- | The recursive Kosaraju algorithm is a depth-first search on the decomposition tree
    rooted by the original GVASS. 

    If the θ condition holds for ANY such decomposition in the tree, then it must hold
    for the original GVASS. 

    If it holds for NO such composition, then it does not hold
    for the original GVASS.
-}
kosaraju' :: [GVASS] -> IO KosarajuResult
kosaraju' vs' = do
    -- Ensure that the GVASSs are all fully decomposed by the SCC decomposition.
    let vs = map makeRigid $ concatMap decompGVASS vs'

    -- Check θ₁,θ₂  for ALL vasses
    -- If it fails, then refine or give up if it cannot be refined
    let checkVASS gvass = do
            -- pPrint gvass
            thetaOne <- θ₁ gvass
            thetaTwo <- θ₂ gvass

            putStrLn ""

            case (thetaOne, thetaTwo) of

                (ThetaOneHolds, ThetaTwoHolds) -> do
                    putStrLn "Kosaraju holds!" 
                    return $ KosarajuHolds gvass

                (ThetaOneHolds, thetaTwoError) -> do
                    putStrLn "Theta one holds!"
                    print thetaTwoError
                    refinedVs <- refineθ₂ gvass thetaTwoError
                    case refinedVs of
                        [g] | g == gvass -> return KosarajuDoesNotHold
                        _   -> kosaraju' refinedVs

                (thetaOneError, _            ) -> do
                    print thetaOneError
                    refinedVs <- refineθ₁ gvass thetaOneError
                    case refinedVs of
                        [g] | g == gvass -> return KosarajuDoesNotHold
                        _   -> kosaraju' refinedVs


        mgood :: [GVASS] -> IO KosarajuResult
        mgood []  = return $ KosarajuHolds $ GVASS []
        mgood [g] = checkVASS g
        mgood (g:xs) = do
            res <- checkVASS g
            case res of
                KosarajuHolds gvass -> return $ KosarajuHolds gvass
                KosarajuDoesNotHold -> mgood xs

    -- If ANY of our GVASSs validate θ, then we retire happy
    mgood vs




--------------------------------------------------------------------------------
-- * Make Rigid Coordinates

makeRigid :: GVASS -> GVASS
makeRigid (GVASS components) = GVASS 
    $ flip fmap components 
    $ \c@Component{..} -> 
        let
            allTrans = flatten <$> (Vector.concat $ Map.elems transitions)

            shouldBeRigid :: Coordinate -> Bool
            shouldBeRigid coord = Vector.all (==0) $ fmap (Vector.! fromIntegral coord) allTrans

            rigidsI    = Set.filter shouldBeRigid initialConstrainedCoords
            rigidsF    = Set.filter shouldBeRigid finalConstrainedCoords
            newValuesI = Map.restrictKeys initialVector rigidsI
            newValuesF = Map.restrictKeys finalVector   rigidsF
            rigids     = rigidsI <> rigidsF
            newValues  = newValuesI <> newValuesF

            in c{  rigidCoords = rigidCoords <> rigids
                ,  rigidValues = rigidValues <> newValues
                , initialConstrainedCoords   = initialConstrainedCoords   Set.\\ rigids
                , finalConstrainedCoords     = finalConstrainedCoords     Set.\\ rigids
                , initialUnconstrainedCoords = initialUnconstrainedCoords Set.\\ rigids
                , finalUnconstrainedCoords   = finalUnconstrainedCoords   Set.\\ rigids
                , initialVector = Map.withoutKeys initialVector rigidsI
                , finalVector   = Map.withoutKeys finalVector   rigidsF
                }

    



--------------------------------------------------------------------------------
-- * θ₁

{- | θ₁ is expressed as follows:
    There exists some (pseudo)-run through the GVASS which uses every edge 
    in every component at least M times (for unbounded M) and gets from the start
    in the first component to the end in the final component.

    The pseudo-run also obeys the restrictions on constrained places at each known point,
    and includes the full adjoinments in every run.
-}

θ₁ :: GVASS -> IO ThetaOneResult
θ₁ gvass@(GVASS components) = do

    res <- runILP gvass
    case res of
        Nothing -> return ThetaOneHasNoSolutions
        Just (vars, bpByVar) -> do

            -- 2. Find the first thing which violates the theta-one condition.
            let firstFailM = listToMaybe $ filter isFail vars
                    where isFail var = all (== 0) $ snd $ bpByVar Map.! var

                maxVal var   = maximum $ fst $ bpByVar Map.! var

            return $ case firstFailM of
                Nothing  -> ThetaOneHolds
                Just var -> ThetaOneFails var (maxVal var)




data VarPosition = Initial | Final
    deriving (Eq, Ord, Show)
data ILPVar = ILPCoord Int VarPosition Coordinate | ILPTrans Int (Name Transition)
    deriving (Eq, Ord, Show)


{-| Given a component, generate and evaluate the Integer Linear Programming
    problem representing θ₁ for that component. We return the basis and the
    period of the solution for that component, which is interpreted separately.
-}
runILP :: GVASS -> IO ( Maybe ( [ILPVar], Map ILPVar ([Integer], [Integer]) ) )
runILP (GVASS components) = do

    let 
        indexedComponents = zip [0..] components 

        allVars :: [(Map ILPVar Integer, Integer)]
        allVars = (concat :: [[a]] -> [a])
            [ concat $ parikhImageConstraints      <$> indexedComponents
            , concat $ kirchoffConstraints         <$> indexedComponents
            , concat $ constrainedValueConstraints <$> indexedComponents
            , concat $ rigidValueConstraints       <$> indexedComponents
            , concat $ adjoinmentConstraints       <$> indexedComponents
            ]

        -- The initial value of each place, plus the delta of each transition
        -- times the number of activations, equals the final value.
        parikhImageConstraints :: (Int, Component) -> [(Map ILPVar Integer, Integer)]
        parikhImageConstraints (compIndex, comp@Component{..}) = 
            let
                flattenedTrans :: Map (Name State) (Vector Transition) -> [(Name Transition, [Integer])]
                flattenedTrans = Vector.toList
                            . fmap (\trans -> (Data.VASS.name trans, Vector.toList $ flatten trans)) 
                            . Vector.concat 
                            . Map.elems

                pairUp :: Functor f => (a, f b) -> f (a, b)
                pairUp (a, b) = fmap (a,) b

                fTrans :: [[(ILPVar, Integer)]]
                fTrans = fmap pairUp $ first (ILPTrans compIndex) <$> flattenedTrans transitions

                allValues :: [[(ILPVar, Integer)]]
                allValues = fTrans ++ 
                    [ [(ILPCoord compIndex Initial i, 1) | i <- range dimension]
                    , [(ILPCoord compIndex Final   i,-1) | i <- range dimension]
                    ]
                
                transposedValues = Data.List.transpose allValues

            in (,0) . Map.fromList <$> transposedValues


        kirchoffConstraints :: (Int, Component) -> [(Map ILPVar Integer, Integer)]
        kirchoffConstraints (compIndex, comp@Component{..}) = 
            let
                adjacencyVal :: Name State -> (Name State, Transition) -> Integer
                adjacencyVal state (prevState, Transition name pre post nextState)
                    = 0
                    + (if state == prevState then   1 else 0)
                    + (if state == nextState then (-1) else 0) 

                pairedTransitions :: [(Name State, Transition)]
                pairedTransitions = decollate transitions

                transVarsWithAdjacency :: Name State -> [(ILPVar, Integer)]
                transVarsWithAdjacency s = (\(ns,t) -> (ILPTrans compIndex (Data.VASS.name t), adjacencyVal s (ns,t))) <$> pairedTransitions

                stateVal :: Name State -> Integer
                stateVal state
                    = 0
                    + (if state == initialState then   1 else 0)
                    + (if state == finalState   then (-1) else 0)


            in (\state -> (Map.fromList $ transVarsWithAdjacency state, stateVal state)) <$> Vector.toList states

        constrainedValueConstraints :: (Int, Component) -> [(Map ILPVar Integer, Integer)]
        constrainedValueConstraints (compIndex, comp@Component{..}) =
            let
                initialConstraints = flip fmap (Set.toList initialConstrainedCoords)
                    $ \coord -> (Map.singleton (ILPCoord compIndex Initial coord) 1, initialVector Map.! coord)

                finalConstraints   = flip fmap (Set.toList finalConstrainedCoords)
                    $ \coord -> (Map.singleton (ILPCoord compIndex Final coord) 1, finalVector Map.! coord)

            in initialConstraints ++ finalConstraints

        rigidValueConstraints :: (Int, Component) -> [(Map ILPVar Integer, Integer)]
        rigidValueConstraints (compIndex, comp@Component{..}) = concat 
            [
                [ (Map.singleton (ILPCoord compIndex Initial coord) 1, val)
                , (Map.singleton (ILPCoord compIndex Final   coord) 1, val)
                ]
            | coord <- Set.toList rigidCoords
            , let val = rigidValues Map.! coord 
            ]

        adjoinmentConstraints :: (Int, Component) -> [(Map ILPVar Integer, Integer)]
        adjoinmentConstraints (compIndex, comp@Component{..}) =
            case adjoinment of
            Nothing         -> []
            Just adjoinment ->  
                (\(coord, delta) -> (
                    Map.fromList 
                        [ (ILPCoord  compIndex    Final   coord, -1)
                        , (ILPCoord (compIndex+1) Initial coord,  1)
                        ]
                    , delta)
                ) <$> zip (range dimension) (Vector.toList adjoinment)

                    
        varsList :: [ILPVar]
        varsList = (concat :: [[a]] -> [a])
            [ (concat :: [[a]] -> [a]) 
                [ ILPCoord i Initial <$> range dimension
                , ILPCoord i Final   <$> range dimension
                , transitions
                    & Map.elems 
                    & map Vector.toList
                    & concat
                    & map Data.VASS.name
                    & map (ILPTrans i)
                ]
            | (i, comp@Component{..}) <- zip [0..] components
            ]

        allVarsDense = first (`makeDense` varsList) <$> allVars

    -- putStrLn "=====THETA1 INPUT====="
    -- pPrint $ allVars
    -- putStrLn "=====THETA1 INPUT (RAW)====="
    -- pPrint $ allVarsDense


    results <- ldn Nothing allVarsDense
    
    let (bases, periods) = case results of
            Homogeneous      p -> ([], p)
            NonHomogeneous b p -> (b , p)

        varBasesOrNothing   = transpose bases   ++ repeat []
        varPeriodsOrNothing = transpose periods ++ repeat [0]

        isRelevant:: ILPVar -> Bool
        isRelevant = (\case
            ILPCoord compIndex Initial coord | coord `elem` (initialUnconstrainedCoords $ components !! compIndex) -> True
            ILPCoord compIndex Final   coord | coord `elem` (  finalUnconstrainedCoords $ components !! compIndex) -> True
            ILPTrans _         _                                                                                   -> True
            _                                                                                                      -> False
            )

    -- putStrLn "=====THETA1 RESULTS====="
    -- pPrint
    --     ( filter isRelevant varsList
    --     , Map.filterWithKey (\k a -> isRelevant k)
    --         $ Map.fromList 
    --         $ zip varsList $ zip varBasesOrNothing varPeriodsOrNothing
    --     )
        
    return $ case (bases, periods) of
        ([], _)  -> Nothing
        _        -> Just 
                    ( filter isRelevant varsList
                    , Map.filterWithKey (\k a -> isRelevant k)
                        $ Map.fromList 
                        $ zip varsList $ zip varBasesOrNothing varPeriodsOrNothing
                    )




{-| We take the result of running θ₁ and use it to refine the GVASS.
    If some transition t has no period, then we replace the GVASS with (n+1) 
    copies, each replacing our bounded component with a series of components, 
    each having [0..n] incidences of t.

    If some coordinate c has no period, it cannot be unbounded; we replace the GVASS 
    with (n+1) copies, each having c as initially constrained to [0..n].
-}
refineθ₁ :: GVASS -> ThetaOneResult -> IO [GVASS]

-- Don't bother if there are no solutions
refineθ₁ g ThetaOneHasNoSolutions = return [g]

refineθ₁ g@(GVASS components) (ThetaOneFails (ILPCoord cindex pos coord) maxVal) = do
    let component@Component{..} = components !! cindex

    case pos of 
        Initial -> do
            putStrLn "Refining by constraining on all possible initial values..."
        -- Refine by all possible constraints on initial coordinate
            return [ modifyComponent g cindex (constrainInitial coord val) | val <- [0..maxVal] ]

        Final -> do
            putStrLn "Refining by constraining on all possible final values..."
        -- Refine by all possible constraints on final coordinate
            return [ modifyComponent g cindex (constrainFinal coord val) | val <- [0..maxVal] ]

    
refineθ₁ g@(GVASS components) (ThetaOneFails (ILPTrans cindex tname) maxVal) = do
    putStrLn "Refining by exploding a bounded transition..."

    let c@Component{..} = components !! cindex

        Just transition = Data.List.find (\(Transition tn _ _ _) -> tn == tname)
                        $ Data.List.concatMap Vector.toList
                        $ Map.elems transitions

        --tSparse = makeSparseCoords $ flatten transition

        applyToAllButLast f (x:y:xs) = f x : applyToAllButLast f (y:xs)
        applyToAllButLast f ls       = ls

        makeChain i component = case i of
            0 -> [component']
            n -> applyToAllButLast (setAdjoinment (Just $ flatten transition))
                $  [setInitial initialVector freeComponent]
                ++ replicate (fromIntegral n - 1) freeComponent
                ++ [setFinal finalVector freeComponent]

            where freeComponent = unconstrainAll component'
                  component'    = removeTransition tname component

    return $ map (\i -> expandComponent g cindex (makeChain i)) [0..maxVal]



--------------------------------------------------------------------------------
-- * θ₂
    
{-| θ₂ is expressed as follows:
    There exists some run through the VASS which allows us to get from q to q,
    strictly increasing ALL non-constrained places
    AND
    some run through the inverted VASS which allows us to get from q' to q',
    strictly decreasing ALL non-constrained places.
-}
θ₂ :: GVASS -> IO ThetaTwoResult
θ₂ (GVASS components) = do
    -- Either YES, or there is some place which is bounded everywhere
    -- If the latter, return the component, place, and bound

    -- For EACH component:
    -- Construct the Karp-Miller tree
    let trees :: [(KarpMillerTree, KarpMillerTree)]
        trees = map buildKarpMillerTree components
    let indexedResults = zip3 [0..] components trees 

    -- Look for the first component which does not satisfy θ₁
    let 
        firstFailM = listToMaybe $ mapMaybe isFail indexedResults
        
        -- TODO: Tidy this up (but it seems correct now)
        isFail :: (Int, Component, (KarpMillerTree, KarpMillerTree)) -> Maybe (Int, Direction, Coordinate, Integer)
        isFail (i, comp@Component{..}, (forwardTree, backwardTree)) = let

            isBoring = Vector.length states == 1 && Vector.null (mconcat $ Map.elems transitions)

            forwardPlace  = (i, Forward, forwardTree, ) . minimum . Set.toList
                <$> findFullyBounded (fromIntegral $ Set.size initialConstrainedCoords) forwardTree

            backwardPlace = (i, Backward, backwardTree, ) . minimum . Set.toList
                <$> findFullyBounded (fromIntegral $ Set.size finalConstrainedCoords) backwardTree

            in  {-if isBoring then trace("component "++ show i ++" is boring") Nothing else-} (forwardPlace >> backwardPlace)
                <&> findMaximum
                <&> \(compIndex, dir, coord, maxVal) -> case dir of
                    Forward   -> (compIndex, dir, Set.toList initialConstrainedCoords !! (fromIntegral coord), maxVal)
                    Backward  -> (compIndex, dir, Set.toList   finalConstrainedCoords !! (fromIntegral coord), maxVal)


        -- Find the maximum value of the known constrained place
        findMaximum :: (Int, Direction, KarpMillerTree, Coordinate) -> (Int, Direction, Coordinate, Integer)
        findMaximum (i, dir, tree, coord) = (i, dir, coord, m)
            where m = fromFinite . getP coord
                    $ maximumBy (comparing (getP coord)) tree

        getP :: Coordinate -> ExtConf -> Nat
        getP coord conf = vec conf Vector.! (fromIntegral coord)


    -- If there is no such component, then θ₁ holds.
    -- Otherwise, we report it back so that we can refine on it.
    return $ case firstFailM of
        Nothing -> ThetaTwoHolds
        Just (i, direction, coord, maxVal) -> BoundedCoord
            { cindex    = i
            , direction = direction
            , coord     = coord
            , maxVal    = maxVal
            } 


{-| With the Karp-Miller tree, we can actually test against a much stronger property:
     Iff we cannot hit (ω,ω,ω,...) then there is at least one place which is constrained EVERYWHERE.
-}
buildKarpMillerTree :: Component -> (KarpMillerTree, KarpMillerTree)
buildKarpMillerTree Component{..} = do

    -- We have to reduce the dimensionality - we only want to consider the 
    -- constrained coordinates, for the purposes of θ₂.

    let 
        -- Convert the sparseVector into a simple vector, 
        -- ensuring the coordinates are in the right order
        initial :: Conf
        initial = Configuration initialState (Vector.fromList $ Map.elems initialVector)

        final :: Conf
        final   = Configuration finalState (Vector.fromList $ Map.elems finalVector)
    
    
        reducedTransitions :: Set Coordinate -> Map (Name State) (Vector Transition)
        reducedTransitions s = fmap (reduceTrans s) <$> transitions 
            where
                reduceTrans :: Set Coordinate -> Transition -> Transition
                reduceTrans s t@Transition{..} = t
                    { pre       = project s pre
                    , post      = project s post
                    }

        vassInitial = VASS 
                { dimension = fromIntegral $ Set.size initialConstrainedCoords
                , states = states
                , places = Vector.fromList 
                         $ fmap (coerce . show)
                         $ Set.toList initialConstrainedCoords 
                , transitions = reducedTransitions initialConstrainedCoords
                }

        vassFinal = reverse $ VASS 
                { dimension = fromIntegral $ Set.size finalConstrainedCoords 
                , states = states
                , places = Vector.fromList
                        $ fmap (coerce.show)
                        $ Set.toList finalConstrainedCoords
                , transitions = reducedTransitions finalConstrainedCoords
                }
    
    (,)
        (karpMillerTree initial vassInitial)
        (karpMillerTree final   vassFinal)


-- | Identify those coordinates which are bounded EVERYWHERE.
-- (Kosaraju suggests that we can know that at least one coordinate is bounded in all runs.)
-- This should terminate as soon as all omegas are found.
findFullyBounded :: Integer -> KarpMillerTree -> Maybe (Set Coordinate)
findFullyBounded dimension tree = findFullyBounded' dimension tree (allCoords dimension)
    where 
        findFullyBounded' :: Integer -> KarpMillerTree -> (Set Coordinate) -> Maybe (Set Coordinate)
        findFullyBounded' dimension (Node (Configuration q vec) children) boundedCoords = do

            let omegaCoords     = coordsWhere (== Omega) vec
            let remainingCoords = boundedCoords Set.\\ omegaCoords 

            if Set.null remainingCoords 
            then Nothing
            else foldM (flip (findFullyBounded' dimension)) remainingCoords children


refineθ₂ :: GVASS -> ThetaTwoResult -> IO [GVASS]
refineθ₂ g@(GVASS components) BoundedCoord{..} = let
    comp@Component{..} = components !! cindex
    in case direction of

        Forward ->
            if  | coord `elem` finalUnconstrainedCoords -> do
                    putStrLn "Refining by constraining on all possible final values..."
                    return [ modifyComponent g cindex (constrainFinal coord c) | c <- [0..maxVal] ]
                | coord `elem` finalConstrainedCoords   -> do
                    putStrLn "Refining by bounding the place inside the component..."
                    putStrLn $ printf "   Bounding %d to [0..%d]" coord maxVal
                    return [ expandComponent g cindex $ boundCoord coord maxVal ]
                | otherwise -> 
                    error $ "Place " ++ show cindex ++ " was neither final constrained nor unconstrained"
        Backward -> 
            if  | coord `elem` initialUnconstrainedCoords -> do
                    putStrLn "Refining by constraining on all possible initial values..."
                    return [ modifyComponent g cindex (constrainInitial coord c) | c <- [0..maxVal] ]
                | coord `elem` initialConstrainedCoords -> do
                    putStrLn "Refining by bounding the place inside the component..."
                    putStrLn $ printf "   Bounding %d to [0..%d]" coord maxVal
                    return [ expandComponent g cindex $ boundCoord coord maxVal ]
                | otherwise -> 
                    error $ "Place " ++ show cindex ++ " was neither initial constrained nor unconstrained"




-- Create a new component (pair) which forces some value to be constrained within a bound.
boundCoord :: Coordinate -> Integer -> Component -> [Component]
boundCoord coord maxVal v@Component{..} = let

    -- Our vector position values are zero indexed.
    vecCoord :: Int
    vecCoord = fromIntegral coord

    a :: Integer
    a = case Map.lookup coord initialVector of
        Nothing -> error "??"
        Just x -> x

    a' :: Integer
    a' = case Map.lookup coord finalVector of
        Nothing -> error "??"
        Just x -> x

    crossStateName :: (Name State) -> Integer -> Name State
    crossStateName state val = coerce newName
        where 
            newName = printf "%s_%i:%i" state' coord val :: String
            state'  = coerce state :: String

    -- The cross product of all states with all values.
    crossStates :: Vector (Name State)
    crossStates = Vector.fromList 
            [ crossStateName state val 
            | state <- Vector.toList states 
            , val   <- [0..maxVal] 
            ]


    crossTransitions :: Map (Name State) (Vector Transition)
    crossTransitions = Map.fromList [
            ( crossStateName state val
            , Vector.fromList 
                [ Transition 
                    { name = coerce (printf "%s_%i:%i" (coerce name::String) coord val :: String)
                    , pre  = pre  Vector.// [(vecCoord, 0)]
                    , post = post Vector.// [(vecCoord, 0)]
                    , nextState = crossStateName nextState (val+delta)
                    }
                | trans@Transition{..} <- Vector.toList $ transitions Data.GVASS.SCC.!@ state
                , let delta = flatten trans Vector.! vecCoord
                -- Only allow when the pre/post does not take us outside the range
                , pre Vector.! vecCoord <= val
                , val + delta <= maxVal
                ]
            )
            | state   <- Vector.toList states
            , val     <- [0..maxVal]
            ]

    v' = Component
        { dimension    = dimension
        , states       = crossStates
        , transitions  = crossTransitions
        , initialState = crossStateName initialState a
        , finalState   = crossStateName finalState   a'
        , rigidCoords  = rigidCoords <> [coord]
        , rigidValues  = rigidValues <> [(coord, a)]
        , initialConstrainedCoords = Set.delete coord initialConstrainedCoords
        , initialUnconstrainedCoords = initialUnconstrainedCoords
        , finalConstrainedCoords   = Set.delete coord finalConstrainedCoords
        , finalUnconstrainedCoords = finalUnconstrainedCoords
        , initialVector = Map.delete coord initialVector
        , finalVector   = Map.delete coord finalVector
        , adjoinment = Just $ makeDenseCoords [(coord, a' - a)] dimension
        }

    v'' = Component
        { dimension    = dimension
        , states       = ["μ"]
        , transitions = []
        , initialState = "μ"
        , finalState  = "μ"
        , rigidCoords = []
        , rigidValues = []
        , initialConstrainedCoords = []
        , initialUnconstrainedCoords = allCoords dimension
        , finalConstrainedCoords = []
        , finalUnconstrainedCoords = allCoords dimension
        , initialVector = []
        , finalVector = []
        , adjoinment = adjoinment -- Use the adjoinment from our original component
    }

    in [v', v'']




data ThetaOneResult = ThetaOneHolds 
                    | ThetaOneFails ILPVar Integer
                    | ThetaOneHasNoSolutions
                    deriving (Eq, Show)

data ThetaTwoResult = ThetaTwoHolds 
                    | BoundedCoord { cindex :: Int, direction :: Direction, coord :: Coordinate, maxVal :: Integer}
        deriving (Eq, Show)

data KosarajuResult = KosarajuHolds GVASS | KosarajuDoesNotHold
        deriving (Eq, Show)

data Direction = Forward | Backward
        deriving (Eq, Show)



--------------------------------------------------------------------------------
-- Utility Functions

-- Add in missing values
makeDense :: Ord a => Map a Integer -> [a] -> [Integer]
makeDense sparse = fmap (\p -> Map.findWithDefault 0 p sparse)

coordsWhere :: Eq a => (a -> Bool) -> Vector a -> Set Coordinate
coordsWhere f vec
        = Set.fromList
        $ fmap fromIntegral
        $ fmap fst
        $ filter (f.snd)
        $ Vector.toList
        $ Vector.indexed vec


-- Strip out all unused values
project :: Eq a => Set Coordinate -> Vector a -> Vector a
project set vec = catMaybes 
        $ fmap (\(i,s) -> toMaybe (i `elem` set) s) 
        $ fmap (first fromIntegral)
        $ Vector.indexed vec 


{-# INLINE toMaybe #-}
toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x


catMaybes :: (Applicative f, Foldable f, Monoid (f a)) => f (Maybe a) -> f a
catMaybes = mconcat . foldr poss []
        where 
            poss :: Applicative f => Maybe a -> [f a] -> [f a]
            poss (Just x) s = pure x : s
            poss Nothing  s = s


unzip2D :: [[(a,b)]] -> ([[a]], [[b]])
unzip2D = unzip . (unzip <$>)

zip2D :: [[a]] -> [[b]] -> [[(a,b)]]
zip2D as bs = uncurry zip <$> zip as bs


decollate :: (Ord k, Eq a) 
    => Map k (Vector a) 
    -> [(k, a)]
decollate m = [ (a, b) | (a,as) <- Map.toList m, b <- Vector.toList as]