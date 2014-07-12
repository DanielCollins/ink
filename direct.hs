module Direct (Expression(..), Variable) where

type Variable = String

data Expression = Reference Variable
                | Lambda Variable Expression
                | Combination Expression Expression


