module Day16.Puzzle1 where

import Relude
import qualified Prelude
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Text as Text
import Day10.Puzzle1 (liftEitherMessage)

type Parser = Parsec.Parsec Void Text

data Field = Field { name :: Text, isValid :: Int -> Bool } 
instance Prelude.Show Field where show Field{name} = "(Field { name: " <> toString name <> ", isValid: <fn> })"
instance Prelude.Eq Field where f1 == f2 = name f1 == name f2

data Contents = Contents { fields :: [Field], yourTicket :: [Int], nearbyTickets :: [[Int]] } deriving (Show, Eq)

parseFields :: Parser [Field] 
parseFields = Parsec.sepEndBy parseField newline 

parseField :: Parser Field
parseField = do
  name <- Text.pack <$> many (alphaNumChar <|> char' ' ')
  _ <- char' ':'
  _ <- many $ char' ' '
  
  ranges <- Parsec.sepBy parseRange (string' " or ")
  let isValid x = any (\(lbound, ubound) -> lbound <= x && x <= ubound) ranges
  pure $ Field { name, isValid }

  where

    parseRange :: Parser (Int, Int)
    parseRange = do
      lbound <- Lexer.decimal 
      _ <- char' '-'
      ubound <- Lexer.decimal
      pure (lbound, ubound)

parseTicket :: Parser [Int]
parseTicket = Parsec.sepBy1 Lexer.decimal (char' ',')

-- parseContents :: Parser Contents 
parseContents = do
  fields <- parseFields
  _ <- some (void spaceChar <|> void (string' "your ticket:"))
  yourTicket <- parseTicket
  _ <- some (void spaceChar <|> void (string' "nearby tickets:"))
  nearbyTickets <- Parsec.sepEndBy parseTicket space1
  pure (fields, yourTicket, nearbyTickets)

  -- pure $ Contents {fields, yourTicket, nearbyTickets}

-- fileContents :: IO Contents 
fileContents = do
  contents <- readFileText "./puzzle16.txt"
  liftEitherMessage $ Parsec.runParser parseContents "puzzle16.txt" contents
