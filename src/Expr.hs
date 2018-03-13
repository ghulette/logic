module Expr where

data Id = String

data Value = IntValue Int
           | BoolValue Bool

intValue :: Value -> Int
intValue (IntValue x) = x
intValue _            = undefined

boolValue :: Value -> Bool
boolValue (BoolValue x) = x
boolValue _             = undefined

data Expr = Lit Value
          | Var Id
          | Neg Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Eq Expr Expr
          | Lt Expr Expr
          | Gt Expr Expr

data Type = IntType
          | BoolType

typeCheck :: (Id -> Type) -> Expr -> Maybe Type
typeCheck _ (Lit (BoolValue _)) = return BoolType
typeCheck _ (Lit (IntValue _)) = return IntType
typeCheck env (Var x) = return (env x)
typeCheck env (Neg e) = do
  IntType <- typeCheck env e
  return IntType
typeCheck env (Add e1 e2) = do
  IntType <- typeCheck env e1
  IntType <- typeCheck env e2
  return IntType
typeCheck env (Sub e1 e2) = do
  IntType <- typeCheck env e1
  IntType <- typeCheck env e2
  return IntType
typeCheck env (Mul e1 e2) = do
  IntType <- typeCheck env e1
  IntType <- typeCheck env e2
  return IntType
typeCheck env (Eq e1 e2) = do
  IntType <- typeCheck env e1
  IntType <- typeCheck env e2
  return BoolType
typeCheck env (Lt e1 e2) = do
  IntType <- typeCheck env e1
  IntType <- typeCheck env e2
  return BoolType
typeCheck env (Gt e1 e2) = do
  IntType <- typeCheck env e1
  IntType <- typeCheck env e2
  return BoolType

eval :: (Id -> Value) -> Expr -> Value
eval _ (Lit val) = val
eval env (Var x) = env x
eval env (Neg e) = IntValue . negate . intValue . (eval env) $ e
