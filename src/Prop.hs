{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Prop where

import           Data.List (foldl', intersect, partition, union)
import           Data.Text (Text)
import           Prelude   hiding (negate)

data Prop a = Atom a
            | FFalse
            | FTrue
            | Not (Prop a)
            | And (Prop a) (Prop a)
            | Or  (Prop a) (Prop a)
            | Imp (Prop a) (Prop a)
            | Iff (Prop a) (Prop a)
            | Forall Ident (Prop a)
            | Exists Ident (Prop a)
            deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

type Valuation a = a -> Bool

empty :: Valuation a
empty _ = error "Empty valuation"

extend :: Eq a => Valuation a -> a -> Bool -> Valuation a
extend v x b q = if q == x then b else v q

valuations :: Eq a => [a] -> [Valuation a]
valuations []     = return empty
valuations (x:xs) = do
  v <- valuations xs
  [extend v x True, extend v x False]

eval :: Prop a -> Valuation a -> Bool
eval (Atom x) v  = v x
eval FFalse _    = False
eval FTrue _     = True
eval (Not p) v   = not (eval p v)
eval (And p q) v = eval p v && eval q v
eval (Or p q) v  = eval p v || eval q v
eval (Imp p q) v = not (eval p v) || eval q v
eval (Iff p q) v = eval p v == eval q v

atoms :: Prop a -> [a]
atoms = foldr (:) []

rewrite :: (Prop a -> Prop a) -> Prop a -> Prop a
rewrite r a@(Atom x) = r a
rewrite r FFalse     = r FFalse
rewrite r FTrue      = r FTrue
rewrite r (Not p)    = r (Not (rewrite r p))
rewrite r (And p q)  = r (And (rewrite r p) (rewrite r q))
rewrite r (Or p q)   = r (Or (rewrite r p) (rewrite r q))
rewrite r (Imp p q)  = r (Imp (rewrite r p) (rewrite r q))
rewrite r (Iff p q)  = r (Iff (rewrite r p) (rewrite r q))

simplify :: Prop a -> Prop a
simplify = rewrite rw
  where rw (Not FFalse)   = FTrue
        rw (Not FTrue)    = FFalse
        rw (Not (Not p))  = p
        rw (And _ FFalse) = FFalse
        rw (And FFalse _) = FFalse
        rw (And p FTrue)  = p
        rw (And FTrue p)  = p
        rw (Or p FFalse)  = p
        rw (Or FFalse p)  = p
        rw (Or _ FTrue)   = FTrue
        rw (Or FTrue _)   = FTrue
        rw (Imp FFalse _) = FTrue
        rw (Imp _ FTrue)  = FTrue
        rw (Imp FTrue p)  = p
        rw (Imp p FFalse) = Not p
        rw (Iff p FTrue)  = p
        rw (Iff FTrue p)  = p
        rw (Iff p FFalse) = Not p
        rw (Iff FFalse p) = Not p
        rw p@_            = p

conjuncts :: Prop a -> [Prop a]
conjuncts FTrue     = []
conjuncts (And p q) = conjuncts p ++ conjuncts q
conjuncts p@_       = [p]

conjoin :: [Prop a] -> Prop a
conjoin []       = FTrue
conjoin (fm:fms) = foldl' And fm fms

disjuncts :: Prop a -> [Prop a]
disjuncts FFalse   = []
disjuncts (Or p q) = disjuncts p ++ disjuncts q
disjuncts p@_      = [p]

disjoin :: [Prop a] -> Prop a
disjoin []       = FFalse
disjoin (fm:fms) = foldl' Or fm fms

tautology :: Ord a => Prop a -> Bool
tautology fm = all (eval fm) (valuations $ atoms fm)

unsatisfiable :: Ord a => Prop a -> Bool
unsatisfiable = tautology . Not

satisfiable :: Ord a => Prop a -> Bool
satisfiable = not . unsatisfiable

nnf :: Prop a -> Prop a
nnf = simplify . nn
  where
    nn (And p q)       = nn p `And` nn q
    nn (Or p q)        = nn p `Or` nn q
    nn (Imp p q)       = nn (Not p) `Or` nn q
    nn (Iff p q)       = (nn p `And` nn q) `Or` (nn (Not p) `And` nn (Not q))
    nn (Not (Not p))   = nn p
    nn (Not (And p q)) = nn (Not p) `Or` nn (Not q)
    nn (Not (Or p q))  = nn (Not p) `And` nn (Not q)
    nn (Not (Imp p q)) = nn p `And` nn (Not q)
    nn (Not (Iff p q)) = (nn p `And` nn (Not q)) `Or` (nn (Not p) `And` nn q)
    nn p@_             = p

isNegative (Not _) = True
isNegative _       = False
isPositive = not . isNegative

negate (Not p) = p
negate p       = Not p

disjoint :: Eq a => [a] -> [a] -> Bool
disjoint s1 s2 = null $ intersect s1 s2

setDnf :: Eq a => Prop a -> [[Prop a]]
setDnf FFalse      = []
setDnf FTrue       = [[]]
setDnf (p `And` q) = [ union x y | x <- setDnf p, y <- setDnf q ]
setDnf (p `Or` q)  = union (setDnf p) (setDnf q)
setDnf p@_         = [[p]]

subsetEq s1 s2 = all (\x -> elem x s2) s1
setEq s1 s2 = subsetEq s1 s2 && subsetEq s2 s1
subset s1 s2 = subsetEq s1 s2 && not (setEq s1 s2)

trivial :: Eq a => [Prop a] -> Bool
trivial lits = not $ disjoint pos (map negate neg)
  where (pos,neg) = partition isPositive lits

dnf :: Eq a => Prop a -> Prop a
dnf = disjoin . map conjoin . simplSetDnf
  where simplSetDnf = subsume . filter (not . trivial) . setDnf . nnf
        subsume fms = filter (\d -> not $ any (\d' -> subset d' d) fms) fms

newtype Ident = Ident { getIdent :: Text } deriving (Eq,Ord,Show)
data Term = Var Ident | Fn Ident [Term] deriving (Eq,Ord,Show)
data Rel = Rel Ident [Term] deriving (Eq,Ord,Show)
type FOL = Prop Rel
