module Day9.Puzzle1 where

import Relude
import Day9

ans :: IO Int
ans = do
  contents <- fileContents
  case firstInvalidNumber contents of
    Just a -> pure a
    Nothing -> error "No solution found."
