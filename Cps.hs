module Cps (cpsTransform, cpsInvoke, Katom(Absorb)) where

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
  show (Procedure p k b) = "(λ (" ++ p ++ " " ++ k ++
     ") " ++ show b ++ ")"

instance Show Katom where
  show (Continuation p b) = "(λ (" ++ p ++
    ") " ++ show b ++ ")"
  show (Kreference v) = v
  show Absorb = "ABSORB"

instance Show Call where
  show (Application p a k) = "(" ++ show p ++ " " ++ show a ++
     " " ++ show k ++ ")"
  show (Invocation k a) = "(" ++ show k ++ " " ++ show a ++ ")"

cpsInvoke :: Expression -> Katom -> Call
cpsInvoke (Combination (Combination a b) c) k =
  cpsInvoke (Combination a b) $
            Continuation "p" $
                        cpsInvoke c $
                                  Continuation "a" $
                                               Application (Ureference "p")
                                                           (Ureference "a")
                                                            k
cpsInvoke (Combination a (Combination b c)) k =
  cpsInvoke (Combination b c) $
            Continuation "v" $
                         Application (cpsTransform a) (Ureference "v") k
cpsInvoke (Combination a b) k = Application (cpsTransform a) (cpsTransform b) k
cpsInvoke e k = Invocation k (cpsTransform e)

cpsTransform :: Expression -> Uatom
cpsTransform (Reference r) = Ureference r
cpsTransform (Lambda p b) = Procedure p "k" $ cpsInvoke b $ Kreference "k"
cpsTransform (Combination a b) = Procedure "v" "k" $ cpsInvoke (Combination a b)
                                                               (Kreference "k")

