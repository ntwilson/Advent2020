module Operators where
  
import Relude

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) fn = fmap (fmap fn)
infixl 4 <$$>

(<&&>) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<&&>) = flip (<$$>)
infixr 4 <&&>

(<<$) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
f1 <<$ f2 = fmap f1 . f2
infixl 4 <<$

(&>>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
(&>>) = flip (<<$)
infixr 4 &>>

