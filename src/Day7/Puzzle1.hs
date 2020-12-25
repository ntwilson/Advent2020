module Day7.Puzzle1 where

import Relude
import Day7
import qualified Data.Set as Set

ans :: IO Int
ans = length . colorsWith "shiny gold" <$> policies 

colorsWith :: Text -> [BagPolicy] -> [Text]
colorsWith c bagPolicies = execState (go c) Set.empty & toList
  where
    go :: Text -> State (Set Text) ()
    go bagColor = do
      colorsSoFar <- get
      let 
        bagsContainingThisColor = bagPolicies & filter (contents >>> any ((== bagColor) . hue))   
        newColors = fromList (color <$> bagsContainingThisColor) `Set.difference` colorsSoFar
      modify (<> newColors)
      traverse_ go newColors


