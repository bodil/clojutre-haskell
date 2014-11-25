module Literally.Clojure where

import Prelude hiding (foldr, sum)
import Data.Monoid
import Data.Foldable

data List a = Nil | Cons a (List a)

foo :: List Integer
foo = Cons 1 (Cons 2 (Cons 3 Nil))

bar :: List Integer
bar = Cons 4 (Cons 5 (Cons 6 Nil))

car :: List a -> a
car (Cons a _) = a

cdr :: List a -> List a
cdr (Cons _ d) = d

-- try this first without the (Show a) restriction
instance (Show a) => Show (List a) where
  show Nil = "Nil"
  show (Cons a d) = "(" ++ show a ++ " . " ++ show d ++ ")"

mapl :: (a -> b) -> List a -> List b
mapl _ Nil = Nil
mapl fn (Cons a d) = Cons (fn a) (mapl fn d)

instance Functor List where
  fmap = mapl

incList :: List Int -> List Int
incList = fmap (1 +)

concatl :: List a -> List a -> List a
concatl Nil l2 = l2
concatl (Cons a d) l2 = Cons a (concatl d l2)

instance Monoid (List a) where
  mempty = Nil
  mappend = concatl

mapcat :: Monoid a => (t -> a) -> List t -> a
mapcat _ Nil = mempty
mapcat f (Cons a Nil) = f a
mapcat f (Cons a d) = mappend (f a) (mapcat f d)

instance Foldable List where
  foldMap = mapcat

dupe :: a -> List a
dupe a = Cons a (Cons a Nil)

suml :: List Int -> Int
suml = foldr (+) 0
