module AoCArray where

import Relude
import Operators
import Data.Array (Array)
import qualified Data.Array as Array


arrayFromListStartingAt :: Int -> [e] -> Array Int e
arrayFromListStartingAt lbound lst = Array.listArray (lbound, lbound + length lst - 1) lst

arrayToList :: Array.Ix i => Array i b -> [b]
arrayToList = snd <<$ Array.assocs

(!) :: Array.Ix i => Array.Array i e -> i -> Maybe e
ar ! i =
  let (lbound, ubound) = Array.bounds ar
  in if lbound <= i && i <= ubound then Just ((Array.!) ar i) else Nothing 

(!?) :: Array.Ix i => Maybe (Array i b) -> i -> Maybe b
ar !? i = ar >>= (! i)
