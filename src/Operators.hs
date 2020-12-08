module Operators where
  
import Relude

(#) :: a -> (a -> b) -> b
(#) = flip ($)
infixl 0 #

(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) = flip (<$>)
infixl 4 <#>
