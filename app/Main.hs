module Main where
    
import qualified Duvet

import Kosaraju.Cov
import Data.VASS.Coverability.KarpMiller.Duvet

-- TODO: Replace this with a "proper" reachability frontend
main :: IO ()
main = Duvet.runDuvet [kosChecker, karpMillerChecker]