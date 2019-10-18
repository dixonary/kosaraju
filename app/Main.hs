module Main where
    
import qualified Duvet

import Kosaraju.Cov
import Kosaraju
import Data.GVASS.Examples
import Data.VASS.Coverability.KarpMiller.Duvet

import System.IO.Silently

-- TODO: Replace this with a "proper" reachability frontend
main :: IO ()
main = Duvet.runDuvet [kosChecker, karpMillerChecker]

--main = silence $ kosaraju (ex5 currentVal) >>= print


currentVal = 30
