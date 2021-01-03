module Day8 where

import Relude
import AoCArray
import qualified Prelude
import Data.Array (Array, listArray)
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Set as Set

data Instruction 
  = Accumulate Int
  | Jump Int
  | NullOp Int
  deriving (Show)

data ProgramState = ProgramState { accumulator :: Int, programCounter :: Int, instructionsEvaluated :: Set Int }
type Program = Array Int Instruction 

type Parser = Parsec.Parsec Void Text 

fileContents :: IO (Array Int Text)
fileContents = do
  fileLines <- lines <$> readFileText "./puzzle8.txt"
  pure $ listArray (1, length fileLines) fileLines

fileInstructions :: IO Program
fileInstructions = do
  contents <- fileContents
  forM contents $ \line -> do
    case Parsec.runParser parseInstruction "puzzle8.txt" line of 
      Left errs -> error $ toText $ Parsec.errorBundlePretty errs
      Right instructions -> pure instructions

parseInstruction :: Parser Instruction
parseInstruction = Parsec.choice [ parseAccumulate, parseJump, parseNullOp ]
  where
    parseAccumulate = do
      _ <- string' "acc "
      Accumulate <$> parseCount

    parseJump = do
      _ <- string' "jmp "
      Jump <$> parseCount

    parseNullOp = do
      _ <- string' "nop "
      NullOp <$> parseCount
    
    parseCount = do 
      multiplier <- (char' '+' $> 1) <|> (char' '-' $> -1)
      count <- Lexer.decimal
      pure $ multiplier * count


data EvaluationTermination  
  = NormalTermination ProgramState
  | InfiniteLoop ProgramState 

instance Show EvaluationTermination where
  show (NormalTermination st) = 
    "Program terminated normally.\nCurrent accumulator: " <> show (accumulator st)
  show (InfiniteLoop st) = 
    "Infinite loop detected at instruction: " <> show (programCounter st) <> ".\nCurrent accumulator: " <> show (accumulator st)

eval :: Program -> ProgramState -> Either EvaluationTermination ProgramState
eval prog st = 
  case prog ! programCounter st of
    Nothing -> Left $ NormalTermination st
    _ | programCounter st `Set.member` instructionsEvaluated st -> Left $ InfiniteLoop st
    Just (Accumulate i) -> pure $ st 
      { programCounter = programCounter st + 1 
      , accumulator = accumulator st + i 
      , instructionsEvaluated = Set.insert (programCounter st) (instructionsEvaluated st)
      }
    Just (Jump i) -> pure $ st 
      { programCounter = programCounter st + i 
      , instructionsEvaluated = Set.insert (programCounter st) (instructionsEvaluated st)
      } 
    Just (NullOp _) -> pure $ st 
      { programCounter = programCounter st + 1
      , instructionsEvaluated = Set.insert (programCounter st) (instructionsEvaluated st)
      }
