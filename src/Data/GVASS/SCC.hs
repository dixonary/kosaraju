module Data.GVASS.SCC (decomposeSCC) where

import Data.GVASS

{- | Perform the Strongly Connected Component Decomposition.

    If some component is not a SCC, then we refine it by replacing the component
    with every possible route through the strongly connected components within
    it.

    This must be performed for every non-SC component in the GVASS, and so
    there is a good chance of exponential blowup.

-}
decomposeSCC :: GVASS -> [GVASS]
decomposeSCC = undefined