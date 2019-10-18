{-# LANGUAGE FlexibleContexts #-}

{-| This is an implementation of a linear diophantine equation solver by Levent Erkok.

It has been modified by me (Alex Dixon) to work with cvc4 and not z3 as a comparison .-}
module LDN where

import Data.SBV
import Data.SBV.Trans.Control
import Control.Concurrent.Async (async, waitAny, waitAnyCancel)
import Documentation.SBV.Examples.Existentials.Diophantine (Solution(..))

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
basis m = extractModels <$> allSatWith cvc4 {-[z3{solverSetOptions = [SetLogic $ QF_LIA]}, cvc4]-} cond
    where 
        cond = do 
            as <- mkExistVars  n
            bs <- mkForallVars n
            return $ ok as .&&  (ok bs .=> as .== bs .|| sNot (bs `less` as))
        n = if null m then 0 else length (head m)
        ok xs = sAny (.> 0) xs .&& sAll (.>= 0) xs .&& sAnd [sum (zipWith (*) r xs) .== 0 | r <- m]
        as `less` bs = sAnd (zipWith (.<=) as bs) .&& sOr (zipWith (.<) as bs)

-- sbvWithAny :: Provable a => [SMTConfig] -> (SMTConfig -> a -> IO b) -> a -> IO (Solver, b)
-- sbvWithAny []      _    _ = error "SBV.withAny: No solvers given!"
-- sbvWithAny solvers what a = snd `fmap` (mapM try solvers >>= waitAnyCancel)
--    where 
--     try s = async $ what s a >>= \r -> return (name (solver s), r)

-- allSatWithAny = (`sbvWithAny` allSatWith)