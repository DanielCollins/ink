module Direct (Expression(..), Variable) where

type Variable = String

data Expression = Reference Variable
                | Lambda Variable Expression
                | Combination Expression Expression

instance Show Expression where
  show (Reference v) = v
  show (Lambda v b) = "(LAMBDA (" ++ v ++ ") " ++ (show b) ++ ")"
  show (Combination a b) = "(" ++ (show a) ++ " " ++
    (show b) ++ ")" 

