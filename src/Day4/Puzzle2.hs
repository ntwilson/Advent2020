module Day4.Puzzle2 where

import Relude
import Day4


import qualified Text.Megaparsec as Parsec
import qualified Text.Megaparsec.Char as Parsec
import qualified Text.Megaparsec.Char.Lexer as Lexer

ans :: IO Int
ans = length <$> passports parseKeyValue

satisfies :: Show a => (a -> Bool) -> Parser a -> Parser a
satisfies fn p = Parsec.try $ do
  val <- p
  if fn val 
  then pure val
  else fail ("Failed to parse: " <> show val <> " failed to satisfy predicate")

parseBirthYear :: Parser PassportField
parseBirthYear = Parsec.try $ do 
  _ <- Parsec.string' "byr:" 
  BirthYear . show <$> satisfies (\yr -> (1920::Int) <= yr && yr <= 2002) Lexer.decimal

parseIssueYear :: Parser PassportField
parseIssueYear = Parsec.try $ do
  _ <- Parsec.string' "iyr:"
  IssueYear . show <$> satisfies (\yr -> (2010::Int) <= yr && yr <= 2020) Lexer.decimal

parseExpirationYear :: Parser PassportField
parseExpirationYear = Parsec.try $ do
  _ <- Parsec.string' "eyr:"
  ExpirationYear . show <$> satisfies (\yr -> (2020::Int) <= yr && yr <= 2030) Lexer.decimal

parseHeight :: Parser PassportField
parseHeight = Parsec.try $ do
  _ <- Parsec.string' "hgt:"
  Height . show <$> (parseHeightCm <|> parseHeightIn)
  where 
    parseHeightCm = do
      height <- satisfies (\cm -> (150::Int) <= cm && cm <= 193) Lexer.decimal
      _ <- Parsec.string' "cm"
      pure height
    
    parseHeightIn = do
      height <- satisfies (\inch -> 59 <= inch && inch <= 76) Lexer.decimal
      _ <- Parsec.string' "in"
      pure height

parseHairColor :: Parser PassportField
parseHairColor = Parsec.try $ do
  _ <- Parsec.string' "hcl:"
  _ <- Parsec.char '#'
  HairColor <$> traverse (const Parsec.hexDigitChar) ([1..6]::[Int])

parseEyeColor :: Parser PassportField
parseEyeColor = Parsec.try $ do
  _ <- Parsec.string' "ecl:"
  EyeColor . toString <$> Parsec.choice (Parsec.string' <$> ["amb","blu","brn","gry","grn","hzl","oth"])

parsePassportID :: Parser PassportField
parsePassportID = Parsec.try $ do
  _ <- Parsec.string' "pid:"
  PassportID <$> traverse (const Parsec.numberChar) ([1..9]::[Int])
  
parseCountryID :: Parser PassportField
parseCountryID = Parsec.try $ do 
  _ <- Parsec.string' "cid:"
  CountryID <$> Parsec.some (Parsec.alphaNumChar <|> Parsec.symbolChar <|> Parsec.punctuationChar)

parseKeyValue :: Parser PassportField
parseKeyValue = Parsec.try $
  Parsec.choice 
    [ parseBirthYear, parseIssueYear, parseExpirationYear, parseHeight 
    , parseHairColor, parseEyeColor, parsePassportID, parseCountryID 
    ]
