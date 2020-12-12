module Main (main) where

import Relude
import qualified Day1.Puzzle1 as D1P1
import qualified Day1.Puzzle2 as D1P2
import qualified Day2.Puzzle1 as D2P1
import qualified Day2.Puzzle2 as D2P2
import qualified Day3.Puzzle1 as D3P1
import qualified Day3.Puzzle2 as D3P2
import qualified Day4.Puzzle1 as D4P1


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

  putTextLn "-------------\nDay3:"
  d3q1 <- D3P1.ans
  putTextLn ("  #1: " <> show d3q1)
  d3q2 <- D3P2.ans
  putTextLn ("  #2: " <> show d3q2)

  putTextLn "-------------\nDay4:"
  d4q1 <- D4P1.ans
  putTextLn ("  #1: " <> show d4q1)
