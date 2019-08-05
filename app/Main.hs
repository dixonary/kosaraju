module Main where

import Kosaraju
import Data.GVASS.Examples
import Text.Pretty.Simple

main :: IO ()
main = kosaraju ex4 >>= pPrint