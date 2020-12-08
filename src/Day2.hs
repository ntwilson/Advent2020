module Day2 where

import Relude 
import Text.Megaparsec (Parsec)
import qualified Data.Text as Text
import qualified Text.Megaparsec as Parsec
import qualified Text.Megaparsec.Char as Parsec
import qualified Text.Megaparsec.Char.Lexer as Lexer

data Requirements = Requirements { requiredChar :: Char, occurrencesMin :: Int, occurrencesMax :: Int } deriving (Show)
data PasswordLine = PasswordLine { policy :: Requirements, password :: Text } deriving (Show)

type PasswordParser = Parsec Void Text 

contents :: IO [Text]
contents = lines <$> readFileText "./puzzle2.txt"

passwords :: IO [PasswordLine]
passwords = do
  file <- contents
  let result = traverse (Parsec.runParser passwordParser "Puzzle2.txt") file
  case result of 
    Left err -> error $ toText $ Parsec.errorBundlePretty err
    Right pwd -> pure pwd

passwordParser :: PasswordParser PasswordLine
passwordParser = do
  occurrencesMin <- Lexer.decimal 
  _ <- Parsec.char '-'
  occurrencesMax <- Lexer.decimal
  _ <- Parsec.space
  requiredChar <- Parsec.alphaNumChar
  _ <- Parsec.many (Parsec.spaceChar <|> Parsec.char ':')
  password <- Text.pack <$> Parsec.many Parsec.alphaNumChar
  pure $ PasswordLine { password, policy = Requirements { requiredChar, occurrencesMin, occurrencesMax } }
