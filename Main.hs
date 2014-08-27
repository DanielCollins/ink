module Main (main) where

import Direct (Expression(..), Variable)
import Cps    (cpsInvoke, Katom(Absorb))

main :: IO ()
main = print $
  cpsInvoke (Combination (Lambda "x" (Reference "x"))
                         (Lambda "x" (Reference "x")))
            Absorb

