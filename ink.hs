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

data Stack a = EmptyStack
             | StackEntry a (Stack a)

type State = (CpsExpression, Stack CpsExpression, Stack CpsExpression)

push :: a -> Stack a -> Stack a
push value stack = StackEntry value stack 

pop :: Stack a -> (Stack a, Maybe a)
pop EmptyStack = (EmptyStack, Nothing)
pop (StackEntry v s) = (s, Just v)

evalUatom :: Uatom -> CpsExpression
evalUatom atom = A atom

evalKatom :: Katom -> CpsExpression
evalKatom atom = B atom

evalCall :: Call -> CpsExpression
evalCall call = C call

step :: State -> State
step s = s

main :: IO ()
main = putStrLn "hi"

