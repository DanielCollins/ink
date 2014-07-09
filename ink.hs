type Variable = String

data Expression = Reference Variable 
                | Lambda Variable Expression
                | Combination Expression Expression 

type Kvariable = String

data Uatom = Procedure Variable Kvariable Call
           | Ureference Variable

data Katom = Continuation Variable Call
           | Kreference Variable

data Call = Application Uatom Uatom Katom
          | Invocation Katom Uatom

data CpsExpression = A Uatom
                   | B Katom
                   | C Call

main :: IO ()
main = putStrLn "hi"

