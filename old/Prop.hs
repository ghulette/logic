import Prelude hiding (foldr)
import Data.List (nub,transpose)
import Data.Foldable (Foldable,foldr)
import qualified Data.Foldable as Foldable
import Text.PrettyPrint.Boxes

type Var = String

data Formula a = PFalse
               | PTrue
               | Atom a
               | Not (Formula a)
               | And (Formula a) (Formula a)
               | Or (Formula a) (Formula a)
               | Impl (Formula a) (Formula a)
               | Iff (Formula a) (Formula a)
               | Forall Var (Formula a)
               | Exists Var (Formula a)
               deriving Eq

false = PFalse
true = PTrue
atom = Atom
neg = Not
(/\) = And
infixl 9 /\
(\/) = Or
infixl 7 \/
(==>) = Impl
infixr 5 ==>
(<==>) = Iff
infix 4 <==>

parens s = "(" ++ s ++ ")"

instance Show a => Show (Formula a) where
  show PFalse = "false"
  show PTrue = "true"
  show (Atom a) = "atom " ++ show a
  show (Not p) = "neg " ++ show p
  show (And p q) = parens $ show p ++ " /\\ " ++ show q
  show (Or p q) = parens $ show p ++ " \\/ " ++ show q
  show (Impl p q) = parens $ show p ++ " ==> " ++ show q
  show (Iff p q) = parens $ show p ++ " <==> " ++ show q
  

conjuncts :: Formula a -> [Formula a]
conjuncts (And p q) = conjuncts p ++ conjuncts q
conjuncts fm = [fm]

disjuncts :: Formula a -> [Formula a]
disjuncts (Or p q) = disjuncts p ++ disjuncts q
disjuncts fm = [fm]

antecedent :: Formula a -> Formula a
antecedent (Impl p _) = p

consequent :: Formula a -> Formula a
consequent (Impl _ q) = q

instance Functor Formula where
  fmap _ PFalse = PFalse
  fmap _ PTrue = PTrue
  fmap f (Atom a) = Atom (f a)
  fmap f (Not p) = Not (fmap f p)
  fmap f (And p q) = And (fmap f p) (fmap f q)
  fmap f (Or p q) = Or (fmap f p) (fmap f q)
  fmap f (Impl p q) = Impl (fmap f p) (fmap f q)
  fmap f (Iff p q) = Iff (fmap f p) (fmap f q)
  fmap f (Forall x p) = Forall x (fmap f p)
  fmap f (Exists x p) = Exists x (fmap f p)

instance Foldable Formula where
  foldr f y PFalse = y
  foldr f y PTrue = y
  foldr f y (Atom x) = f x y
  foldr f y (Not p) = foldr f y p
  foldr f y (And p q) = foldr f (foldr f y q) p
  foldr f y (Or p q) = foldr f (foldr f y q) p
  foldr f y (Impl p q) = foldr f (foldr f y q) p
  foldr f y (Iff p q) = foldr f (foldr f y q) p
  foldr f y (Forall _ p) = foldr f y p
  foldr f y (Exists _ p) = foldr f y p

atoms :: Eq a => Formula a -> [a]
atoms = nub . Foldable.toList

valuations :: Eq a => [a] -> [a -> Bool]
valuations [] = return (const $ error "Not found")
valuations (x:xs) = do
  b <- [False,True]
  f <- valuations xs
  return (\y -> if x == y then b else f y)

eval :: Formula a -> (a -> Bool) -> Bool
eval PFalse _ = False
eval PTrue _ = True
eval (Atom x) e = e x
eval (Not p) e = not (eval p e)
eval (And p q) e = eval p e && eval q e
eval (Or p q) e = eval p e || eval q e
eval (Impl p q) e = not (eval p e) || eval q e
eval (Iff p q) e = eval p e == eval q e

printTruthTable :: Formula String -> IO ()
printTruthTable fm = printBox $ hsep 2 left $ map (vcat center1 . map text) cols
  where ats = atoms fm
        boolTok True = "T"
        boolTok False = "F"
        row v = boolTok (eval fm v) : map (boolTok . v) ats
        header = "formula" : ats
        rows = header : map row (valuations ats)
        cols = transpose rows

tautology :: Eq a => Formula a -> Bool
tautology fm = and $ map (eval fm) (valuations (atoms fm))

unsatisfiable :: Eq a => Formula a -> Bool
unsatisfiable fm = tautology (neg fm)

satisfiable :: Eq a => Formula a -> Bool
satisfiable fm = not (unsatisfiable fm)