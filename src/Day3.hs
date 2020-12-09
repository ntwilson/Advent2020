module Day3 where

import Relude
import Operators

contents :: IO [MapRow]
contents = parseLines =<< (toString <$$> lines <$> readFileText "./puzzle3.txt")

data MapTile = Tree | Open deriving (Show, Eq, Ord)
type MapRow = [MapTile]

parseLines :: MonadFail m => [String] -> m [MapRow]
parseLines = traverse parseLine 

parseLine :: MonadFail m => String -> m MapRow
parseLine line = do
  row <- traverse parseTile line
  pure $ cycle row
  where
    parseTile '#' = pure Tree
    parseTile '.' = pure Open
    parseTile c = fail ("Found '" <> [c] <> "', but was expecting '#' or '.'")
