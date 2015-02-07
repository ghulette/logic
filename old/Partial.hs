module Partial where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

newtype Fpf a b = Fpf (Map a b)

instance Functor (Fpf a) where
  fmap f (Fpf m) = Fpf (fmap f m)

empty :: Fpf a b
empty = Fpf Map.empty

fpf :: Ord a => [a] -> [b] -> Fpf a b
fpf xs ys = Fpf (Map.fromList (zip xs ys))

graph :: Fpf a b -> [(a,b)]
graph (Fpf m) = Map.toList m

undefine :: Ord a => a -> Fpf a b -> Fpf a b
undefine x (Fpf m) = Fpf (Map.delete x m)

(|->) :: Ord a => a -> b -> Fpf a b -> Fpf a b
(x |-> y) (Fpf m) = Fpf (Map.insert x y m)

(|=>) :: Ord a => a -> b -> Fpf a b
x |=> y = (x |-> y) empty

domain :: Fpf a b -> Set a
domain (Fpf m) = Map.keysSet m

range :: Ord b => Fpf a b -> Set b
range (Fpf m) = Set.fromList (Map.elems m)
