module Operators where
  
import Relude

(#) :: a -> (a -> b) -> b
(#) = flip ($)

(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) = flip (<$>)
