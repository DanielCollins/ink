type Variable = String

data Expression = Reference Variable
                | Lambda Variable Expression
                | Combination Expression Expression 

type Kvariable = String

data Uatom = Procedure Variable Kvariable Call
           | Ureference Variable
  deriving (Show)

data Katom = Continuation Variable Call
           | Kreference Variable
  deriving (Show)

data Call = Application Uatom Uatom Katom
          | Invocation Katom Uatom
  deriving (Show)

cpsTransform :: Expression -> Katom -> Call
cpsTransform (Reference r) k = Invocation k $ Ureference r
cpsTransform (Lambda p b) k = Invocation k $ Procedure p
                                             "k" $
                                cpsTransform b $ Kreference "k"
cpsTransform (Combination a b) k = cpsTransform  a $ Continuation "k" $ cpsTransform b k

main :: IO ()
main = print $ cpsTransform (Reference "a") $ Kreference "halt" 

