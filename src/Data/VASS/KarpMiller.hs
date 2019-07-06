{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, OverloadedLists, BangPatterns #-}

module Data.VASS.KarpMiller where

import Data.VASS
import Data.VASS.Shared
import Data.VASS.KarpMiller.ExtendedNaturals
import Data.Functor
import Data.Vector (Vector)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.List (partition, foldl')
import qualified Control.Monad.State as MTL
import Debug.Trace (traceShowId)
import Data.Foldable (find)
import Data.Maybe
import qualified Data.Vector as DV


data Problem = Problem VASS Configuration Configuration
    deriving (Show)


isSafe :: Problem -> IO ()
isSafe (Problem v i t) = do
    print (Problem v i t)
    putStrLn $ if (i,v) `covers` t then "Unsafe" else "Safe"



{-| The VASS, with starting point, covers the target, iff 
we can reach some configuration which is at least the target.
-}
covers :: Configured VASS -> Configuration -> Bool
covers cvas target =
    let
        kmt = constructKarpMillerTree cvas
        gt  = find (extend target <=) kmt
    in isJust gt


-- * Karp-Miller Tree Construction

constructKarpMillerTree :: Configured VASS -> KMTree
constructKarpMillerTree (initial, v@(VASS states transitions))= let

    initial' :: ExtConf
    initial' = extend initial

    emptyMaxsets :: Map (Label State) MaxSet
    emptyMaxsets = Map.fromList $ zip states (repeat [])

    -- Given a set of ancestors (from root to current leaf) and an incident transition, 
    -- produce the next node in the sequence.

    generateNode :: [ExtConf] -> ExtConf -> MTL.State (Map (Label State) MaxSet) KMTree
    generateNode ancestors ec@(ExtConf currentState currentVec) = do
        
        maxSet <- MTL.get

        -- We should quit early if the current state is subsumed by one we have already found
        
        if (maxSet ! currentState) `contains` currentVec
        then return $ DeadEnd ec
        else do

            let 
                ancestors' :: [ExtConf]
                ancestors' = ec_ext : ancestors

                -- We consider only lesser ancestors whose state matches the current state.
                -- This is implied by the (<=) relation on Extended Configurations.
                lesserAncestors :: [ExtConf]
                lesserAncestors = filter (<= ec) ancestors 

                -- Introduce omegas in places where values have strictly increased.
                addOmegas :: ExtConf -> ExtConf -> ExtConf
                addOmegas (ExtConf _ vecNew) (ExtConf q vecOld) = 
                    ExtConf q $ DV.zipWith (\old new -> if new > old then Omega else new) vecOld vecNew

                -- Extend the configuration with omegas in places where we
                -- have strictly increased.
                ec_ext :: ExtConf
                ec_ext@(ExtConf _ currentVec') = foldl' addOmegas ec lesserAncestors

                -- Transitions active from the current node.
                activeTrans :: [Transition]
                activeTrans = [ t | (label,t) <- transitions ! currentState
                                  , t `activeFrom` ec_ext]

                -- Apply some transition to the current (extended) vector, getting us
                -- to a new state (and so a new node)
                applyTrans :: Transition -> ExtConf
                applyTrans (pre, post, newState) = 
                    ExtConf newState (currentVec' - (Finite <$> pre) + (Finite <$> post))

            -- Put the current vector into the relevant max set.
            MTL.modify $ Map.insertWith union currentState [currentVec']

            -- Generate the children of this node.
            children <- mapM (generateNode ancestors' . applyTrans) 
                                (DV.fromList activeTrans)

            return $ Node ec_ext children

    in MTL.evalState (generateNode [] initial') emptyMaxsets




data Tree a = Node a (DV.Vector (Tree a))
            | DeadEnd a
    deriving (Functor, Foldable, Traversable, Show)

type KMTree = Tree ExtConf

-- | We keep track of all the highest values that we have seen.
type MaxSet = [ExtVector]

union :: MaxSet -> MaxSet -> MaxSet
a `union` b = foldr insert b a

-- | Insert a vector into the maximum set, if it is large enough.
insert :: ExtVector -> MaxSet -> MaxSet
insert conf maxSet = let
    (lts, nlts) = partition (<= conf) maxSet
    gts = filter (conf <) maxSet
    in if not $ null gts
        then maxSet
        else conf : nlts

-- | If the configuration is less than any element of some object,
-- we say that the object "contains" the configuration.
contains :: Foldable f => f ExtVector -> ExtVector -> Bool
structure `contains` conf = any (conf <=) structure


    {-
    generateNode :: [ExtConf] -> (State, ExtConf,ExtConf) -> MTL.State MaxSet KMTree
    generateNode ancestors@(parent:ps) transition@(newState, pre, post) =
        let
            newC            = parent - pre + post
            -- Find ancestors which are strictly less
            lesserAncestors = filter (<= newC) (ancestors ! q)
            -- Add omegas in those places which are strictly less
            newCO           = foldl' addOmegas newC lesserAncestors

            addOmegas (ExtConf new) (ExtConf anc) =
                ExtConf $ DV.zipWith (\a b -> if b < a then Omega else a) new anc

            activeTransitions = DV.filter (\(pre,post) -> pre <= newCO) transitions

        in do
            maxSet <- get
            if maxSet `contains` newCO
                then return $ DeadEnd newCO
                else do
                    modify $ insert newCO
                    children <- mapM (generateNode (newCO : ancestors)) activeTransitions
                    return $ Node newCO children
    -}
