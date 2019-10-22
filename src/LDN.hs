{-# LANGUAGE FlexibleContexts #-}

{-| This is an implementation of a linear diophantine equation solver by Levent Erkok.

It has been modified by me (Alex Dixon) to check against all installed solvers.-}
module LDN where

import Data.SBV
import Data.SBV.Trans.Control
import Control.Concurrent.Async (async, waitAny, waitAnyCancel)
import Documentation.SBV.Examples.Existentials.Diophantine (Solution(..))
import Data.Maybe (listToMaybe)
import Data.List (intersect)
import Data.Function (on)

import Data.Char(toLower)

import System.Environment.Blank

--------------------------------------------------------------------------------------------------
-- * Solving diophantine equations
--------------------------------------------------------------------------------------------------
-- | ldn: Solve a (L)inear (D)iophantine equation, returning minimal solutions over (N)aturals.
-- The input is given as a rows of equations, with rhs values separated into a tuple.
ldn :: [([Integer], Integer)] -> IO Solution
ldn problem = do 
    solution <- basis (map (map literal) m)
    if homogeneous
    then return $ Homogeneous solution
    else do let ones  = [xs | (1:xs) <- solution]
                zeros = [xs | (0:xs) <- solution]
            return $ NonHomogeneous ones zeros
    where 
        rhs = map snd problem
        lhs = map fst problem
        homogeneous = all (== 0) rhs
        m   | homogeneous = lhs
            | True        = zipWith (\x y -> -x : y) rhs lhs


basis :: [[SInteger]] -> IO [[Integer]]
basis m = do
    solver <- chooseSolver
    extractModels <$> allSatWith solver cond
    where 
        cond = do 
            as <- mkExistVars  n
            bs <- mkForallVars n

            setLogic AUFLIA

            return $ ok as .&&  (ok bs .=> as .== bs .|| sNot (bs `less` as))
        n = if null m then 0 else length (head m)
        ok xs = sAny (.> 0) xs .&& sAll (.>= 0) xs .&& sAnd [sum (zipWith (*) r xs) .== 0 | r <- m]
        as `less` bs = sAnd (zipWith (.<=) as bs) .&& sOr (zipWith (.<) as bs)


instance Eq SMTConfig where (==) = (==) `on` solver
instance Eq SMTSolver where (==) = (==) `on` name
deriving instance Eq Solver

chooseSolver :: IO SMTConfig
chooseSolver = do

    solverEnvironmentVar <- getEnv "KOSARAJU_SOLVER"
    case fmap toLower <$> solverEnvironmentVar of
        Just "z3" -> return z3
        Just "cvc4" -> return cvc4
        _ -> findSolver

findSolver :: IO SMTConfig
findSolver = do
    availableSolvers <- sbvAvailableSolvers

    let usableSolvers = availableSolvers `intersect` [z3, cvc4]
        firstSolver   = listToMaybe usableSolvers

    case firstSolver of
        Nothing -> error $ "You do not have a solver available on your system."
                        <> " Please install either z3 or cvc4 and ensure it is"
                        <> " available on your $PATH."
        Just solver -> return solver