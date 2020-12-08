module Main (main) where

import Relude
import qualified Day1.Puzzle1 as D1P1
import qualified Day1.Puzzle2 as D1P2
import qualified Day2.Puzzle1 as D2P1
import qualified Day2.Puzzle2 as D2P2


main :: IO ()
main = do 
  putTextLn "Day1:"
  d1q1 <- D1P1.ans 
  putTextLn ("  #1: " <> show d1q1)
  d1q2 <- D1P2.ans
  putTextLn ("  #2: " <> show d1q2)

  putTextLn "-------------\nDay2:"
  d2q1 <- D2P1.ans
  putTextLn ("  #1: " <> show d2q1)
  d2q2 <- D2P2.ans
  putTextLn ("  #2: " <> show d2q2)
