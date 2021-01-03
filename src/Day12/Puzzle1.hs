module Day12.Puzzle1 where

import Relude
import Text.Megaparsec as Parsec 
import Text.Megaparsec.Char as Parsec
import Text.Megaparsec.Char.Lexer as Lexer

fileContents :: IO Text
fileContents = readFileText "./puzzle12.txt"

data Instruction = GoNorth Int | GoEast Int | GoSouth Int | GoWest Int | TurnRight Int | TurnLeft Int | GoForward Int

data Direction = North | East | South | West 
  deriving (Show, Eq, Bounded, Enum)

cyclicSucc :: (Eq p, Bounded p, Enum p) => p -> p
cyclicSucc x 
  | x == maxBound = minBound 
  | otherwise = succ x

cyclicPred :: (Eq p, Bounded p, Enum p) => p -> p
cyclicPred x 
  | x == minBound = maxBound 
  | otherwise = pred x

turnRight :: Direction -> Int -> Direction
turnRight initial new = 
  let steps = new `div` 90
  in foldl' (\x _ -> cyclicSucc x) initial [1 .. steps]

turnLeft :: Direction -> Int -> Direction
turnLeft initial new = 
  let steps = new `div` 90
  in foldl' (\x _ -> cyclicPred x) initial [1 .. steps]

data FerryState = FerryState { north :: Int, east :: Int, south :: Int, west :: Int, bearing :: Direction } 

parseInstruction :: Text -> Maybe Instruction
parseInstruction = Parsec.parseMaybe parser
  where
    parser :: Parsec.Parsec Void Text Instruction
    parser = 
      Parsec.choice  
        [ Parsec.string' "N" *> Lexer.decimal <&> GoNorth
        , Parsec.string' "E" *> Lexer.decimal <&> GoEast
        , Parsec.string' "S" *> Lexer.decimal <&> GoSouth
        , Parsec.string' "W" *> Lexer.decimal <&> GoWest
        , Parsec.string' "L" *> Lexer.decimal <&> TurnLeft
        , Parsec.string' "R" *> Lexer.decimal <&> TurnRight
        , Parsec.string' "F" *> Lexer.decimal <&> GoForward
        ]

reducer :: FerryState -> Instruction -> FerryState
reducer ferry (GoNorth i) = ferry { north = north ferry + i }
reducer ferry (GoEast i) = ferry { east = east ferry + i }
reducer ferry (GoSouth i) = ferry { south = south ferry + i }
reducer ferry (GoWest i) = ferry { west = west ferry + i }
reducer ferry (TurnRight i) = ferry { bearing = turnRight (bearing ferry) i }
reducer ferry (TurnLeft i) = ferry { bearing = turnLeft (bearing ferry) i }
reducer ferry (GoForward i) = 
  case bearing ferry of
    North -> reducer ferry $ GoNorth i
    East -> reducer ferry $ GoEast i
    South -> reducer ferry $ GoSouth i
    West -> reducer ferry $ GoWest i

manhattenDistance :: FerryState -> Int
manhattenDistance ferry = abs (north ferry - south ferry) + abs (east ferry - west ferry)

fileInstructions :: IO [Instruction]
fileInstructions = do
  contents <- lines <$> fileContents
  case traverse parseInstruction contents of 
    Nothing -> error "Failed to parse the input for day 12"
    Just a -> pure a

solution :: [Instruction] -> Int 
solution = manhattenDistance . foldl' reducer initialState
  where
    initialState = FerryState { north = 0, east = 0, south = 0, west = 0, bearing = East }

ans :: IO Int
ans = solution <$> fileInstructions
