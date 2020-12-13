{-# LANGUAGE LambdaCase #-}
module Day4.Puzzle1 where

import Relude
import Day4
import qualified Text.Megaparsec as Parsec
import qualified Text.Megaparsec.Char as Parsec

ans :: IO Int
ans = length <$> passports parsePassport


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

fieldsToPassport :: [PassportField] -> Maybe Passport
fieldsToPassport passportVals = do
  birthYear <- viaNonEmpty head $ mapMaybe (\case { BirthYear yr -> Just yr; _ -> Nothing }) passportVals
  issueYear <- viaNonEmpty head $ mapMaybe (\case { IssueYear x -> Just x; _ -> Nothing }) passportVals
  expirationYear <- viaNonEmpty head $ mapMaybe (\case { ExpirationYear x -> Just x; _ -> Nothing }) passportVals
  height <- viaNonEmpty head $ mapMaybe (\case { Height x -> Just x; _ -> Nothing }) passportVals
  hairColor <- viaNonEmpty head $ mapMaybe (\case { HairColor x -> Just x; _ -> Nothing }) passportVals
  eyeColor <- viaNonEmpty head $ mapMaybe (\case { EyeColor x -> Just x; _ -> Nothing }) passportVals
  passportID <- viaNonEmpty head $ mapMaybe (\case { PassportID x -> Just x; _ -> Nothing }) passportVals
  let countryID = viaNonEmpty head $ mapMaybe (\case { CountryID x -> Just x; _ -> Nothing }) passportVals
  pure $ Passport { birthYear, issueYear, expirationYear, height, hairColor, eyeColor, passportID, countryID }

parsePassport :: Parser Passport
parsePassport = do
  -- consumes the first newline at the end of a passport entry
  keysNVals <- Parsec.sepEndBy parseKeyValue Parsec.spaceChar
  case fieldsToPassport keysNVals of 
    Just ppt -> pure ppt
    Nothing -> fail "Could not build a passport out of the given keys and values"