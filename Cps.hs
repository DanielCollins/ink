module Cps (cpsTransform, Katom(Absorb)) where

import Direct (Expression(..), Variable)

type Kvariable = String

data Uatom = Procedure Variable Kvariable Call
           | Ureference Variable

data Katom = Continuation Variable Call
           | Kreference Variable
           | Absorb

data Call = Application Uatom Uatom Katom
          | Invocation Katom Uatom

instance Show Uatom where
  show (Ureference r) = r
  show (Procedure p k b) = "(LAMBDA (" ++ p ++ " " ++ k ++
     ") " ++ show b ++ ")"

instance Show Katom where
  show (Continuation p b) = "(CONTINUATION " ++ p ++
    " " ++ show b ++ ")"
  show (Kreference v) = v
  show Absorb = "ABSORB"

instance Show Call where
  show (Application p a k) = "(" ++ show p ++ " " ++ show a ++
     " " ++ show k ++ ")"
  show (Invocation k a) = "(" ++ show k ++ " " ++ show a ++ ")"

cpsTransform :: Expression -> Katom -> Call
cpsTransform (Reference r) k = Invocation k $ Ureference r
cpsTransform (Lambda p b) k = Invocation k $ Procedure p
                                             "k" $
                                cpsTransform b $ Kreference "k"
cpsTransform (Combination a b) k = cpsTransform  a $ Continuation "k" $ cpsTransform b k


