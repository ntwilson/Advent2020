module Day7.Puzzle2 where

import Relude
import Day7
import qualified Data.Map as Map


ans :: IO Int
ans = sum . Map.elems . bagsContainedBy "shiny gold" <$> policies 

bagsContainedBy :: Text -> [BagPolicy] -> Map Text Int
bagsContainedBy c bagPolicies = execState (go 1 c) Map.empty
  where
    go :: Int -> Text -> State (Map Text Int) ()
    go multiplier bagColor = 
      case find ((bagColor ==) . color) bagPolicies of
        Nothing -> pure ()
        Just policy -> 
          for_ (contents policy) $ \content -> do
            colorsSoFar <- get
            let 
              countOfThisParticularBag = count content * multiplier
              countOfThisColor = countOfThisParticularBag + (Map.lookup (hue content) colorsSoFar & fromMaybe 0)
            modify $ Map.insert (hue content) countOfThisColor
            go countOfThisParticularBag (hue content)


numBagsContainedBy :: Text -> [BagPolicy] -> Int
numBagsContainedBy c bagPolicies = go c
  where
    go :: Text -> Int 
    go bagColor = 
      case find ((bagColor ==) . color) bagPolicies of
        Nothing -> 0
        Just policy -> sum [ count content + (count content * go (hue content)) | content <- contents policy ]
