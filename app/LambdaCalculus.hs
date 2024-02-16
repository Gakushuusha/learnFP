-- an example of the simplest lambda calculus interpreter to explore
module LambdaCalculus where 

data Either a b 
    = Left a
    | Right b

data Bool = True | False
data Maybe a = Nothing | Just a

data E
  = EApp E E
  | EAbs E
  | EVar Int  -- EVar n refers to the variable bound by the nth-innermost abstraction
  | EInt Int  -- literals
  deriving (Show)

eval :: E -> E
eval (EApp fun arg) = case eval fun of
    EAbs body -> eval $ sub 0 body where
        sub n e = case e of
            EApp e1 e2 -> EApp (sub n e1) (sub n e2)
            EAbs e' -> EAbs $ sub (n+1) e'
            EVar n'
                | n == n'    -> arg  -- substitute. arg has no free vars.
                | otherwise  -> EVar n'
            EInt i -> EInt i
    other -> EApp other arg
eval x = x

lambdaCalculus :: IO ()
lambdaCalculus = print $ eval $ EApp (EApp (EAbs (EAbs (EVar 1))) (EInt 42)) (EInt 43)

