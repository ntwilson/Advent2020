module Operators where
  
import Relude

(#) :: a -> (a -> b) -> b
(#) = flip ($)
infixl 0 #

(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) = flip (<$>)
infixl 4 <#>

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) fn = fmap (fmap fn)
infixl 4 <$$>

(<##>) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<##>) = flip (<$$>)
infixl 4 <##>
