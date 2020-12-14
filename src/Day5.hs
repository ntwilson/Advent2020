module Day5 where

import Relude
import Text.Megaparsec
import Text.Megaparsec.Char

contents :: IO [Text]
contents = lines <$> readFileText "./puzzle5.txt"

type Parser = Parsec Void Text 

data Partition = Lower | Upper deriving (Eq, Show)
data Row = 
  Row { rowPartitions :: [Partition], columnPartitions :: [Partition] }
  deriving (Eq, Show)

parseRow :: Parser Row
parseRow = do
  rowPartitions <- traverse (const parseRowPartition) ([1 .. 7]::[Int])
  columnPartitions <- traverse (const parseColumnPartition) ([1 .. 3]::[Int])
  pure $ Row { rowPartitions, columnPartitions }
  where
    parseRowPartition = (char 'F' $> Lower) <|> (char 'B' $> Upper)
    parseColumnPartition = (char 'L' $> Lower) <|> (char 'R' $> Upper)

rows :: IO [Row]
rows = mapMaybe (parseMaybe parseRow) <$> contents

data Range = Range { lBound :: Int, uBound :: Int } 

startingRowRange :: Range
startingRowRange = Range 0 127
startingColumnRange :: Range
startingColumnRange = Range 0 7

idOfSeat :: Row -> Int 
idOfSeat seatDescriptor = 
  let
    rowID = findIndex startingRowRange $ rowPartitions seatDescriptor
    colID = findIndex startingColumnRange $ columnPartitions seatDescriptor
  in rowID * 8 + colID 

  where
    findIndex Range { lBound, uBound } (Lower:rest) = findIndex newRange rest
      where newRange = (Range { lBound, uBound = uBound - ((uBound - lBound) `div` 2) - ((uBound - lBound) `rem` 2) })
    findIndex Range { lBound, uBound } (Upper:rest) = findIndex newRange rest
      where newRange = (Range { uBound, lBound = lBound + ((uBound - lBound) `div` 2) + ((uBound - lBound) `rem` 2) })
    findIndex Range { lBound } [] = lBound
