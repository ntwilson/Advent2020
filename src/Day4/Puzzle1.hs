module Day4.Puzzle1 where

import Relude
import Day4
import qualified Text.Megaparsec as Parsec
import qualified Text.Megaparsec.Char as Parsec

ans :: IO Int
ans = length <$> passports parseKeyValue

parseKeyValue :: Parser PassportField
parseKeyValue = Parsec.try $
  Parsec.choice 
    [ Parsec.string "byr:" *> anyString <&> BirthYear
    , Parsec.string "iyr:" *> anyString <&> IssueYear
    , Parsec.string "eyr:" *> anyString <&> ExpirationYear
    , Parsec.string "hgt:" *> anyString <&> Height
    , Parsec.string "hcl:" *> anyString <&> HairColor
    , Parsec.string "ecl:" *> anyString <&> EyeColor
    , Parsec.string "pid:" *> anyString <&> PassportID
    , Parsec.string "cid:" *> anyString <&> CountryID
    ]

  where 
    anyString = Parsec.some (Parsec.alphaNumChar <|> Parsec.symbolChar <|> Parsec.punctuationChar)
