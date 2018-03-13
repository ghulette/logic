module Expr where

data Value = IntVal Integer
           | BoolVal Bool
           deriving (Eq, Show)

data Unary = UNeg | UNot deriving (Eq, Show)

evalUnary :: Unary -> Value -> Value
evalUnary UNeg (IntVal n)  = IntVal (-n)
evalUnary UNot (BoolVal b) = BoolVal (not b)
evalUnary _ _              = undefined

data Binop = BAdd | BSub | BMul | BDiv
           | BAnd | BOr
           | BLt  | BLte | BEq
           deriving (Eq, Show)

evalBinary :: Binop -> Value -> Value -> Value
evalBinary BAdd (IntVal n1) (IntVal n2)   = IntVal (n1 + n2)
evalBinary BSub (IntVal n1) (IntVal n2)   = IntVal (n1 - n2)
evalBinary BMul (IntVal n1) (IntVal n2)   = IntVal (n1 * n2)
evalBinary BDiv (IntVal n1) (IntVal n2)   = IntVal (n1 `div` n2)
evalBinary BAnd (BoolVal b1) (BoolVal b2) = BoolVal (b1 && b2)
evalBinary BOr (BoolVal b1) (BoolVal b2)  = BoolVal (b1 || b2)

data Expr v = Val Value
            | Var v
            | Unary Unary (Expr v)
            | Binop Binop (Expr v) (Expr v)
            deriving (Eq, Show)

eval :: Expr v -> (v -> Value) -> Value
eval (Val val) _          = val
eval (Var x) env          = env x
eval (Unary op e) env     = evalUnary op (eval e env)
eval (Binop op e1 e2) env = evalBinary op (eval e1 env) (eval e2 env)
