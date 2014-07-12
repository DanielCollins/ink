type Variable = String

data Expression = Reference Variable
                | Lambda Variable Expression
                | Combination Expression Expression 

type Kvariable = String

data Uatom = Procedure Variable Kvariable Call
           | Ureference Variable

instance Show Uatom where
  show (Ureference r) = r
  show (Procedure p k b) = "(LAMBDA (" ++ p ++ " " ++ k ++
     ") " ++ show b ++ ")"

data Katom = Continuation Variable Call
           | Kreference Variable
           | Absorb

instance Show Katom where
  show (Continuation p b) = "(CONTINUATION " ++ show p ++
    " " ++ show b ++ ")"
  show (Kreference v) = v
  show Absorb = "ABSORB"

data Call = Application Uatom Uatom Katom
          | Invocation Katom Uatom

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

main :: IO ()
main = print $ cpsTransform (Reference "a") Absorb

