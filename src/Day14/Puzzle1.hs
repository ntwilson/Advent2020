module Day14.Puzzle1 where

import Relude
import Text.Megaparsec (choice, Parsec)
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char (char', string')
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Day13.Puzzle1 (liftEitherMessage)

data MaskBit = Zero | One | NoUpdate deriving (Show, Eq)
type Mask = [] MaskBit

data Instruction 
  = SetValue { address :: Int, value :: Int }
  | SetMask Mask
  deriving (Show)

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
        [ char' 'X' $> NoUpdate
        , char' '0' $> Zero
        , char' '1' $> One
        ]

fileContents :: IO [Text]
fileContents = lines <$> readFileText "./puzzle14.txt"

fileInstructions :: IO [Instruction]
fileInstructions = do
  contents <- fileContents
  traverse (Parsec.runParser parseLine "puzzle14.txt" >>> first Parsec.errorBundlePretty >>> liftEitherMessage) contents