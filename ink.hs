module Main (main) where

import Direct (Expression(..), Variable)
import Cps    (cpsTransform, Katom(Absorb))

main :: IO ()
main = print $ cpsTransform (Combination
   (Lambda "x" (Reference "x"))
   (Lambda "x" (Reference "x")))
   Absorb

