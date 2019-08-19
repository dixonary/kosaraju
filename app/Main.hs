module Main where
    
import qualified Duvet

import Kosaraju.Cov
import Data.VASS.Coverability.KarpMiller.Duvet

main :: IO ()
main = Duvet.runDuvet [kosChecker, karpMillerChecker]