module Day4.Puzzle1 where

import Relude
import Day4

ans :: IO Int
ans = length <$> validPassports

validPassports :: IO [ParsedPassport]
validPassports = filter (/= InvalidPassport) <$> passports