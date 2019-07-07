{-| 
Module: Kosaraju
An implementation of Kosaraju's famous algorithm which implements
the Reachability decision problem over Vector Addition Systems with States.
-}

{-# LANGUAGE OverloadedLists #-}

module Kosaraju where

import Data.SBV
import Documentation.SBV.Examples.Existentials.Diophantine (ldn, Solution(..))

import Data.List ((\\), elemIndex, find, transpose)
import Data.Map (Map, (!))
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.GVASS
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

import Prelude hiding (reverse)

import Data.Coerce

import Text.Printf


--------------------------------------------------------------------------------
-- * TODO:

-- 1. Strongly connected component decomposition
-- 2. Restructure θ₁ to account for the entire system simultaneously

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
kosaraju' vs = do
    -- Check θ₁,θ₂  for ALL vasses
    -- If it fails, then refine or give up if it cannot be refined
    results <- forM vs $ \gvass -> do
        thetaOne <- θ₁ gvass
        thetaTwo <- θ₂ gvass

        case (thetaOne, thetaTwo) of

            (ThetaOneHolds, ThetaTwoHolds) -> return KosarajuHolds

            (ThetaOneHolds, thetaTwoError) -> do
                    let refinedVs = refineθ₂ gvass thetaTwoError
                    case refinedVs of
                        [_] -> return KosarajuDoesNotHold
                        _   -> kosaraju' refinedVs

            (thetaOneError, _            ) -> do
                    let refinedVs = refineθ₁ gvass thetaOneError
                    case refinedVs of
                        [_] -> return KosarajuDoesNotHold
                        _   -> kosaraju' refinedVs
            

    -- If ANY of our GVASSs validate θ, then we retire happy
    return $ if KosarajuHolds `elem` results
        then KosarajuHolds
        else KosarajuDoesNotHold


--------------------------------------------------------------------------------
-- * θ₁

{- | θ₁ is expressed as follows:
    There exists some (pseudo)-run through the GVASS which uses every edge 
    in every component at least M times (for unbounded M) and gets from the start
    in the first component to the end in the final component.

    The pseudo-run also obeys the restrictions on constrained places at each known point.
-}

θ₁ :: GVASS -> IO ThetaOneResult
θ₁ (GVASS components) =
    undefined

{-
    -- 1. Construct ILP problem.
    -- Finding unbounded solutions in the non-homo case is the same as 
    -- finding any solution in the homo (RHS zeroes) case.
    results <- mapM buildILP components
    let indexedResults = zip3 [0..] components results

    -- Look for the first component which does not satisfy θ₁
    let 
        firstFailM = listToMaybe $ mapMaybe isFail indexedResults

        -- We have a fail on any variable where all values are zero.
        isFail ( i, comp, r@(v, b, p) ) = 
            case find (\(v,p) -> all (== 0) p) periodsByVar
            of
                Nothing             -> Nothing
                Just (name, zeroes) -> Just (i, name, maximum $ basesByVar ! name)
                
            where
                periodsByVar = zip v $ transpose p
                basesByVar   = Map.fromList $ zip v $ transpose b

    -- If there is no such component, then θ₁ holds.
    -- Otherwise, we report it back so that we can refine on it.
    case firstFailM of
        Nothing -> return ThetaOneHolds
        Just (cindex, failVar, maxVal) ->
            case failVar of
                Coord i -> return $ ZeroCoord      cindex i maxVal
                Trans t -> return $ ZeroTransition cindex t maxVal



{-| Given a component, generate and evaluate the Integer Linear Programming
    problem representing θ₁ for that component. We return the basis and the
    period of the solution for that component, which is interpreted separately.
-}
buildILP :: Component -> IO ([ThetaOneVariable], [[Integer]], [[Integer]])
buildILP Component{..} = do

    let allTransitions = concatMap (\(s,ts) -> map ((,) s) ts)
                       $ Map.toList transitions

        allInitCoords  = Set.toList initialUnconstrainedCoords
        allFinalCoords = Set.toList finalUnconstrainedCoords

    -- One row in our ILP problem per coordinate of vector
    -- The total change in each coordinate must equal the difference 
    -- between our starting and ending values.
    -- If our place is unconstrained in the input, we must be able to
    -- arbitrarily pump that place down.
    -- If our place is unconstrained in the output, we must be able to
    -- arbitrarily pump that place up.
    let coordRow coord = (lhs, rhs)
            where
            transImages = map ((Vector.! (coord-1)) . collapse . snd . snd) allTransitions
            unconstrained = 
                   map (\c -> if coord == c then  1 else 0) allInitCoords
                ++ map (\c -> if coord == c then -1 else 0) allFinalCoords

            lhs = transImages ++ unconstrained

            i = Map.findWithDefault 0 coord initialVector
            f = Map.findWithDefault 0 coord finalVector

            rhs = f - i

        coordRows = coordRow <$> [1..dimension]


    -- One row in our ILP per state
    -- Kirchoff's law ie inbound = outbound, modulo start/end
    -- Inbound = -1; Outbound = +1
    let stateRow state = (lhs, rhs) 
            where

            transWeight t = inWeight + outWeight
                where
                inWeight   = if nextState (snd (snd t)) == state then -1 else 0
                outWeight  = if fst t                   == state then  1 else 0

            transWeights = map transWeight allTransitions

            unconstrained = flip replicate 0 $
                  Set.size initialUnconstrainedCoords 
                + Set.size finalUnconstrainedCoords

            lhs = transWeights ++ unconstrained

            i = if initialState  == state then  1 else 0
            f = if finalState    == state then -1 else 0

            rhs = f + i

        stateRows = stateRow <$> states

    let rows = coordRows   ++ stateRows


    let cols = transitionNames ++ initialIndices ++ finalIndices
            where transitionNames = Trans . fst <$> concat (Map.elems transitions)
                  initialIndices  = Coord <$> allInitCoords
                  finalIndices    = Coord <$> allFinalCoords

    putStrLn "Coordinate Rows:"
    mapM_ (printf "    %s\n" . show) coordRows
    putStrLn ""

    putStrLn "State Rows:"
    mapM_ (printf "    %s\n" . show) stateRows
    putStrLn ""

    -- TODO: Rework so that we get labelled transitions and labelled coords
    (b, p) <- runLDN rows

    return (cols, b, p)

-}



{-| We take the result of running θ₁ and use it to refine the GVASS.
    If some transition t has no period, then we replace the GVASS with (n+1) 
    copies, each replacing our bounded component with a series of components, 
    each having [0..n] incidences of t.

    If some coordinate c has no period, it cannot be unbounded; we replace the GVASS 
    with (n+1) copies, each having c as initially constrained to [0..n].
-}
refineθ₁ :: GVASS -> ThetaOneResult -> [GVASS]
refineθ₁ g@(GVASS components) ThetaOneHolds  = error "Theta one holds!"
refineθ₁ g@(GVASS components) ZeroCoord {..} = do
    let component@Component{..} = components !! cindex

    -- We need different behaviour if the vertex was initially vs finally unconstrained
    if 
        | coord `elem` initialUnconstrainedCoords ->
        -- Refine by all possible constraints on initial coordinate
            [ modifyComponent g cindex (constrainInitial coord val) | val <- [0..maxVal] ]

        | coord `elem` finalUnconstrainedCoords ->
        -- Refine by all possible constraints on final coordinate
            [ modifyComponent g cindex (constrainFinal coord val) | val <- [0..maxVal] ]

        -- Something's gone wrong!
        | otherwise
            -> error $ unlines $ 
                [ "Tried to constrain a coordinate which was not unconstrained!" 
                , show g
                , show (ZeroCoord{..})
                ]
    
refineθ₁ g@(GVASS components) ZeroTransition {..} = do

    let c@Component{..} = components !! cindex

    let
        makeChain i component = case i of
            0 -> [component]
            n -> [setInitial initialVector freeComponent]
                ++ replicate (fromIntegral n - 1) freeComponent
                ++ [setFinal finalVector freeComponent]

            where freeComponent = unconstrainAll component'
                  component'    = removeTransition trans component

    map (\i -> expandComponent g cindex (makeChain i)) [0..maxVal]



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
        
        -- What is our failure condition?
        isFail :: (Int, Component, (KarpMillerTree, KarpMillerTree)) -> Maybe (Int, Direction, Coordinate, Integer)
        isFail (i, comp@Component{..}, (forwardTree, backwardTree)) = let

            forwardPlace :: Maybe (Int, Direction, KarpMillerTree, Coordinate)
            forwardPlace  = (i, Forward, forwardTree, ) . Set.findMin 
                    <$> findFullyBounded dimension forwardTree
            backwardPlace = (i, Backward, backwardTree, ) . Set.findMin 
                    <$> findFullyBounded dimension backwardTree

            in findMaximum <$> (forwardPlace >> backwardPlace)


        -- Find the maximum value of the known constrained place
        findMaximum :: (Int, Direction, KarpMillerTree, Coordinate) -> (Int, Direction, Coordinate, Integer)
        findMaximum (i, dir, tree, coord) = (i, dir, coord, m)
            where m = fromFinite . getPlace (fromIntegral coord)
                    $ maximumBy (comparing (getPlace (fromIntegral coord))) tree


    -- If there is no such component, then θ₁ holds.
    -- Otherwise, we report it back so that we can refine on it.
    return $ case firstFailM of
        Nothing -> ThetaTwoHolds
        Just (i, direction, coord, maxVal) -> BoundedCoord
            { cindex    = i
            , direction = direction
            , coord     = coord
            , maxVal = maxVal
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

        vassInitial = VASS 
                { dimension = fromIntegral $ Set.size initialConstrainedCoords
                , states = states
                , places = Vector.fromList 
                         $ fmap (coerce . show)
                         $ Set.toList initialConstrainedCoords 
                , transitions = reducedTransitions
                }
            where
                reducedTransitions :: Map (Name State) (Vector Transition)
                reducedTransitions = fmap reduceTrans <$> transitions 
                
                reduceTrans :: Transition -> Transition
                reduceTrans t@Transition{..} = t
                    { pre       = project initialConstrainedCoords pre
                    , post      = project initialConstrainedCoords post
                    }

        vassFinal = reverse $ vassInitial { transitions = reducedTransitions }
            where
                reducedTransitions :: Map (Name State) (Vector Transition)
                reducedTransitions = fmap reduceTrans <$> transitions 
                
                reduceTrans :: Transition -> Transition
                reduceTrans t@Transition{..} = t
                    { pre       = project finalConstrainedCoords pre
                    , post      = project finalConstrainedCoords post
                    }
    
    (,)
        (karpMillerTree initial vassInitial)
        (karpMillerTree final   vassFinal)


-- | Identify those coordinates which are bounded EVERYWHERE.
-- (Kosaraju suggests that we can know that at least one coordinate is bounded in all runs.)
-- This should terminate as soon as all omegas are found.
findFullyBounded :: Integer -> KarpMillerTree -> Maybe (Set Coordinate)
findFullyBounded dimension tree = findFullyBounded' dimension tree [1..dimension]
    where 
        findFullyBounded' :: Integer -> KarpMillerTree -> (Set Coordinate) -> Maybe (Set Coordinate)
        findFullyBounded' dimension (Node (Configuration q vec) children) boundedCoords = do

            let omegaCoords = Set.fromList 
                     $  Vector.toList 
                     $  (+1) 
                    <$> fromIntegral 
                    <$> Vector.findIndices (== Omega) vec

            let remainingCoords = boundedCoords Set.\\ omegaCoords 

            if Set.null remainingCoords 
            then Nothing
            else foldM (flip (findFullyBounded' dimension)) remainingCoords children


refineθ₂ :: GVASS -> ThetaTwoResult -> [GVASS]
refineθ₂ g@(GVASS components) ThetaTwoHolds    = error "Theta two holds!"
refineθ₂ g@(GVASS components) BoundedCoord{..} = let
    Component{..} = components !! cindex
    in case direction of

        {- Process for "forwards" bound:
            Suppose `coord` is final unconstrained:
                Then fix its value to maxVal and make no other changes.
            Suppose 'coord' is final constrained:
                Then replace the component with TWO components

        Process for "backwards" bound:
            Exactly the same but s/final/initial
        -} 

        Forward ->
            if  | coord `elem` finalUnconstrainedCoords ->
                    [ modifyComponent g cindex (constrainFinal coord c) | c <- [0..maxVal] ]
                | coord `elem` finalConstrainedCoords   ->
                    []    -- TODO
                | otherwise -> 
                    error $ "Place " ++ show cindex ++ " was neither final constrained nor unconstrained?"
        Backward -> 
            if  | coord `elem` initialUnconstrainedCoords ->
                    [ modifyComponent g cindex (constrainFinal coord c) | c <- [0..maxVal] ]
                | coord `elem` initialConstrainedCoords ->
                    []     -- TODO
                | otherwise -> 
                    error $ "Place " ++ show cindex ++ " was neither initial constrained nor unconstrained?"

-- | Given a set of rows, print and return the basis and the period.
runLDN :: [([Integer], Integer)] -> IO ([[Integer]], [[Integer]])
runLDN rows = do
    res <- ldn Nothing rows

    let (b,p) = case res of 
            Homogeneous p      -> ([], p)
            NonHomogeneous b p -> (b , p)

    let noSolution = null b || null p

    when noSolution $ putStrLn "There is no solution."
    when (b /= [])  $ do
        putStrLn "Bases:"
        mapM_ (printf "    %s\n" . show) b
    when (p /= [])  $ do
        putStrLn $  "Periods:"
        mapM_ (printf "    %s\n" . show) p

    putStrLn ""

    return (b,p)



data ThetaOneResult = ThetaOneHolds 
                    | ZeroCoord      { cindex :: Int, coord :: Coordinate     , maxVal :: Integer }
                    | ZeroTransition { cindex :: Int, trans :: Name Transition, maxVal :: Integer }
                    deriving (Eq, Show)

data ThetaOneVariable = Coord Int 
                      | Trans Transition
                      deriving (Eq, Show)

data ThetaTwoResult = ThetaTwoHolds 
                    | BoundedCoord { cindex :: Int, direction :: Direction, coord :: Coordinate, maxVal :: Integer}
        deriving (Eq, Show)

data KosarajuResult = KosarajuHolds | KosarajuDoesNotHold
        deriving Eq

data Direction = Forward | Backward
        deriving (Eq, Show)



--------------------------------------------------------------------------------
-- Utility Functions

project :: Eq a => Set Coordinate -> Vector a -> Vector a
project set vec = catMaybes 
        $ fmap (\(i,s) -> toMaybe ((i+1) `elem` set) s) 
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

