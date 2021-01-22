module Main (main) where

import Relude
import qualified Day1.Puzzle1 as D1P1
import qualified Day1.Puzzle2 as D1P2
import qualified Day2.Puzzle1 as D2P1
import qualified Day2.Puzzle2 as D2P2
import qualified Day3.Puzzle1 as D3P1
import qualified Day3.Puzzle2 as D3P2
import qualified Day4.Puzzle1 as D4P1
import qualified Day4.Puzzle2 as D4P2
import qualified Day5.Puzzle1 as D5P1
import qualified Day5.Puzzle2 as D5P2
import qualified Day6.Puzzle1 as D6P1
import qualified Day6.Puzzle2 as D6P2
import qualified Day7.Puzzle1 as D7P1
import qualified Day7.Puzzle2 as D7P2
import qualified Day8.Puzzle1 as D8P1
import qualified Day8.Puzzle2 as D8P2
import qualified Day9.Puzzle1 as D9P1
import qualified Day9.Puzzle2 as D9P2
import qualified Day10.Puzzle1 as D10P1
import qualified Day10.Puzzle2 as D10P2
import qualified Day11.Puzzle1 as D11P1
import qualified Day11.Puzzle2 as D11P2
import qualified Day12.Puzzle1 as D12P1
import qualified Day12.Puzzle2 as D12P2
import qualified Day13.Puzzle1 as D13P1
import qualified Day13.Puzzle2 as D13P2
import qualified Day14.Puzzle1 as D14P1
import qualified Day14.Puzzle2 as D14P2
import qualified Day15.Puzzle1 as D15P1


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
  d4q2 <- D4P2.ans
  putTextLn ("  #2: " <> show d4q2)

  putTextLn "-------------\nDay5:"
  d5q1 <- D5P1.ans
  putTextLn ("  #1: " <> show d5q1)
  d5q2 <- D5P2.ans
  putTextLn ("  #2: " <> show d5q2)

  putTextLn "-------------\nDay6:"
  d6q1 <- D6P1.ans
  putTextLn ("  #1: " <> show d6q1)
  d6q2 <- D6P2.ans
  putTextLn ("  #2: " <> show d6q2)

  putTextLn "-------------\nDay7:"
  d7q1 <- D7P1.ans
  putTextLn ("  #1: " <> show d7q1)
  d7q2 <- D7P2.ans
  putTextLn ("  #2: " <> show d7q2)

  putTextLn "-------------\nDay8:"
  d8q1 <- D8P1.ans
  putTextLn ("  #1: " <> show d8q1)
  d8q2 <- D8P2.ans
  putTextLn ("  #2: " <> show d8q2)

  putTextLn "-------------\nDay9:"
  d9q1 <- D9P1.ans
  putTextLn ("  #1: " <> show d9q1)
  d9q2 <- D9P2.ans
  putTextLn ("  #2: " <> show d9q2)

  putTextLn "-------------\nDay10:"
  d10q1 <- D10P1.ans
  putTextLn ("  #1: " <> show d10q1)
  d10q2 <- D10P2.ans
  putTextLn ("  #2: " <> show d10q2)

  putTextLn "-------------\nDay11:"
  d11q1 <- D11P1.ans
  putTextLn ("  #1: " <> show d11q1)
  d11q2 <- D11P2.ans
  putTextLn ("  #2: " <> show d11q2)

  putTextLn "-------------\nDay12:"
  d12q1 <- D12P1.ans
  putTextLn ("  #1: " <> show d12q1)
  d12q2 <- D12P2.ans
  putTextLn ("  #2: " <> show d12q2)

  putTextLn "-------------\nDay13:"
  d13q1 <- D13P1.ans
  putTextLn ("  #1: " <> show d13q1)
  d13q2 <- D13P2.ans
  putTextLn ("  #2: " <> show d13q2)

  putTextLn "-------------\nDay14:"
  d14q1 <- D14P1.ans
  putTextLn ("  #1: " <> show d14q1)
  d14q2 <- D14P2.ans
  putTextLn ("  #2: " <> show d14q2)

  putTextLn "-------------\nDay15:"
  let d15q1 = D15P1.ans
  putTextLn ("  #1: " <> show d15q1)
