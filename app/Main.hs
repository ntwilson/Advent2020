module Main (main) where

import Relude
import qualified Day1.Puzzle1 as D1P1
import qualified Day1.Puzzle2 as D1P2


main :: IO ()
main = do 
  q1 <- D1P1.ans 
  putTextLn ("#1: " <> show q1)
  q2 <- D1P2.ans
  putTextLn ("#2: " <> show q2)
