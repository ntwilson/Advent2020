module Day12.Puzzle2 where

import Relude
import Text.Megaparsec as Parsec 
import Text.Megaparsec.Char as Parsec
import Text.Megaparsec.Char.Lexer as Lexer

fileContents :: IO Text
fileContents = readFileText "./puzzle12.txt"

data Instruction = GoNorth Int | GoEast Int | GoSouth Int | GoWest Int | TurnRight Int | TurnLeft Int | GoForward Int

data WaypointPosition = WaypointPosition { waynorth :: Int, wayeast :: Int, waysouth :: Int, waywest :: Int }
data FerryState = FerryState { waypoint :: WaypointPosition, north :: Int, east :: Int, south :: Int, west :: Int }

turnRight :: WaypointPosition -> Int -> WaypointPosition
turnRight initial new = 
  case (new `div` 90) `mod` 4 of
    1 -> WaypointPosition { wayeast = waynorth initial, waysouth = wayeast initial, waywest = waysouth initial, waynorth = waywest initial }
    2 -> WaypointPosition { waysouth = waynorth initial, waywest = wayeast initial, waynorth = waysouth initial, wayeast = waywest initial }
    3 -> WaypointPosition { waywest = waynorth initial, waynorth = wayeast initial, wayeast = waysouth initial, waysouth = waywest initial }
    _ -> initial

turnLeft :: WaypointPosition -> Int -> WaypointPosition
turnLeft initial new = 
  case (new `div` 90) `mod` 4 of
    1 -> WaypointPosition { waywest = waynorth initial, waynorth = wayeast initial, wayeast = waysouth initial, waysouth = waywest initial }
    2 -> WaypointPosition { waysouth = waynorth initial, waywest = wayeast initial, waynorth = waysouth initial, wayeast = waywest initial }
    3 -> WaypointPosition { wayeast = waynorth initial, waysouth = wayeast initial, waywest = waysouth initial, waynorth = waywest initial }
    _ -> initial


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
reducer ferry (GoNorth i) = ferry { waypoint = (waypoint ferry) { waynorth = waynorth (waypoint ferry) + i } }
reducer ferry (GoEast i) = ferry { waypoint = (waypoint ferry) { wayeast = wayeast (waypoint ferry) + i } }
reducer ferry (GoSouth i) = ferry { waypoint = (waypoint ferry) { waysouth = waysouth (waypoint ferry) + i } }
reducer ferry (GoWest i) = ferry { waypoint = (waypoint ferry) { waywest = waywest (waypoint ferry) + i } }
reducer ferry (TurnRight i) = ferry { waypoint = turnRight (waypoint ferry) i }
reducer ferry (TurnLeft i) = ferry { waypoint = turnLeft (waypoint ferry) i }
reducer ferry (GoForward i) = 
  ferry 
    { north = north ferry + i * waynorth (waypoint ferry)
    , east = east ferry + i * wayeast (waypoint ferry)
    , south = south ferry + i * waysouth (waypoint ferry)
    , west = west ferry + i * waywest (waypoint ferry)
    }

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
    initialState = FerryState 
      { north = 0, east = 0, south = 0, west = 0
      , waypoint = WaypointPosition { waynorth = 1, wayeast = 10, waysouth = 0, waywest = 0 }
      }

ans :: IO Int
ans = solution <$> fileInstructions
