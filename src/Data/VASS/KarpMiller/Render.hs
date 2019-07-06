{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Data.VASS.KarpMiller.Render where

import Data.VASS.KarpMiller.ExtendedNaturals
import Data.VASS.KarpMiller hiding (Tree(..))
import qualified Data.VASS.KarpMiller as KarpMiller (Tree(..), KMTree)
import Diagrams.Prelude hiding (render)
import Data.Tree (Tree(..))
import qualified Data.Vector as Vector
import Diagrams.Backend.SVG
import Data.Function ((&))
import Graphics.SVGFonts

import Data.VAS.Read (Spec(..))
import Data.VASS (fromVAS)


import Diagrams.TwoD.Layout.Tree


-- | Given a VAS and a filepath, render the Karp-Miller tree as an SVG 
-- and save it to that file.
renderSpec :: Spec -> FilePath -> IO ()
renderSpec (Spec v i t) path = do
    -- VAS -> VASS
    let
      v' = fromVAS v
      i' = ("μ", fromIntegral <$> i)
      km = constructKarpMillerTree $ (i',v')
    render path km

-- | Given a file path, output the generated Karp-Miller tree as an SVG.
render :: FilePath -> KarpMiller.KMTree -> IO ()
render path kmtree = renderSVG path size (scale sf diagram)
    where hsep = let KarpMiller.Node a _ = kmtree in fromIntegral (dim a + 4)
          diagram = renderTree
            (text)
            {-
            (arrowBetween' (with & headLength .~ normalized 0.05
                                 & headGap .~ normalized 0.1
                                 & tailGap .~ normalized 0.1))
            -}
            (arrowBetween' (with & shaftStyle %~ lw (local 0.1)
                                 & headLength .~ local 1
                                 & headGap .~ local 1
                                 & tailGap .~ local 1))
            (symmLayout' (with & slHSep .~ hsep & slVSep .~ 10) (toCTree kmtree))
            # centerXY # pad 1.1

          -- text' t = stroke (textSVG t 1)
          size :: SizeSpec V2 Double
          --size = absolute
          size = dims $ r2 (800,800)

          sf :: Double
          --sf = 4
          sf = 1


toCTree :: KarpMiller.KMTree -> Tree String
toCTree (KarpMiller.Node a cs) = Node (show a) $ Vector.toList $ fmap toCTree cs
toCTree (KarpMiller.DeadEnd a) = Node (show a++" ■") []
