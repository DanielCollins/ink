type Uvariable = String

type Kvariable = String

data Uatom = Procedure Uvariable Kvariable Call
           | Ureference Uvariable

data Katom = Continuation Uvariable Call
           | Kreference Uvariable

data Call = Application Uatom Uatom Katom
          | Invocation Katom Uatom

data Expression = A Uatom
                | B Katom
                | C Call

data Stack a = EmptyStack
             | StackEntry a (Stack a)

type State = (Expression, Stack Expression, Stack Expression)

push :: a -> Stack a -> Stack a
push value stack = StackEntry value stack 

pop :: Stack a -> (Stack a, Maybe a)
pop EmptyStack = (EmptyStack, Nothing)
pop (StackEntry v s) = (s, Just v)

evalUatom :: Uatom -> Expression
evalUatom atom = A atom

evalKatom :: Katom -> Expression
evalKatom atom = B atom

evalCall :: Call -> Expression
evalCall call = C call

step :: State -> State
step s = s

main :: IO ()
main = putStrLn "hi"

