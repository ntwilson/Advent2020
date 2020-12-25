module Day7 where

import Relude
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Text as Text

fileContents :: IO [Text]
fileContents = lines <$> readFileText "./puzzle7.txt"

type Parser = Parsec.Parsec Void Text

data BagContents = BagContents { count :: Int, hue :: Text } deriving (Show)
data BagPolicy = BagPolicy { color :: Text, contents :: [BagContents] } deriving (Show)

parseLine :: Parser BagPolicy 
parseLine = do
  color <- Text.pack <$> Parsec.manyTill (alphaNumChar <|> spaceChar) (string' " bags contain")
  contents <- Parsec.try (string' " no other bags." $> []) <|> Parsec.many parseContents
  pure $ BagPolicy { color, contents }

parseContents :: Parser BagContents 
parseContents = do
  _ <- space
  count <- Lexer.decimal
  _ <- space
  hue <- Text.pack <$> Parsec.manyTill (alphaNumChar <|> spaceChar) (string' " bags" <|> string' " bag")
  _ <- punctuationChar 
  _ <- space
  pure $ BagContents { count, hue }

policies :: IO [BagPolicy]
policies = do
  c <- fileContents
  case traverse (Parsec.runParser parseLine "puzzle6.txt") c of
    Left errs -> error $ toText $ Parsec.errorBundlePretty errs
    Right p -> pure p
