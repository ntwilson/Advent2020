module Day14.Puzzle2 where

import Relude
import Text.Megaparsec (choice, Parsec)
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char (char', string')
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Day13.Puzzle1 (liftEitherMessage)
import qualified Numeric as Int
import Data.Char (digitToInt, intToDigit)
import qualified Data.IntMap.Strict as Map

data MaskBit = Floating | One | NoUpdate deriving (Show, Eq)
type Mask = [] MaskBit

data Instruction 
  = SetValue { address :: Int, value :: Int }
  | SetMask Mask
  deriving (Show)

type Program = [] Instruction 

data ProgramState = ProgramState { memory :: IntMap Int, currentMask :: Mask }

reducer :: ProgramState -> Instruction -> ProgramState 
reducer st (SetMask mask) = st { currentMask = mask }
reducer st@ProgramState{currentMask, memory} SetValue{address, value} = 
  st { memory = applyMask currentMask address & foldl' (\mem addrs -> Map.insert addrs value mem) memory }

execute :: ProgramState -> Program -> ProgramState
execute = foldl' reducer 

showBinary :: Int -> String
showBinary i = Int.showIntAtBase 2 intToDigit i ""

readBinary :: String -> Maybe Int
readBinary i = Int.readInt 2 (`elem` ['0','1']) digitToInt i & listToMaybe <&> fst 

applyMask :: Mask -> Int -> [Int]
applyMask mask i = fromMaybe i . readBinary <$> updatedBinaries
  where 
    updatedBinaries :: [String]
    updatedBinaries = sequence $ zipWithRight updateChar binary mask 

    binary = ['0' | _ <- [1 .. 100]] <> Int.showIntAtBase 2 intToDigit i "" 

    updateChar _digit Floating = ['0', '1']
    updateChar _digit One = ['1']
    updateChar digit NoUpdate = [digit]

zipWithRight :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithRight f xs ys = reverse $ zipWith f (reverse xs) (reverse ys)

type Parser = Parsec Void Text

parseLine :: Parser Instruction 
parseLine = 
  choice [parseValue, parseMask]
  where
    parseValue = Parsec.try $ do
      _ <- string' "mem["
      address <- Lexer.decimal
      _ <- string' "] = "
      value <- Lexer.decimal
      pure $ SetValue { address, value }

    parseMask = Parsec.try $ do
      _ <- string' "mask = "
      SetMask <$> Parsec.many parseMaskChar

    parseMaskChar = do 
      choice 
        [ char' 'X' $> Floating
        , char' '0' $> NoUpdate
        , char' '1' $> One
        ]

fileContents :: IO [Text]
fileContents = lines <$> readFileText "./puzzle14.txt"

fileInstructions :: IO Program
fileInstructions = do
  contents <- fileContents
  traverse (Parsec.runParser parseLine "puzzle14.txt" >>> first Parsec.errorBundlePretty >>> liftEitherMessage) contents

solution :: Program -> Int 
solution = sum . Map.elems . memory . execute (ProgramState { memory = Map.empty, currentMask = [] }) 

ans :: IO Int
ans = solution <$> fileInstructions 