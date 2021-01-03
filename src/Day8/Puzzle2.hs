module Day8.Puzzle2 where

import Relude
import AoCArray
import qualified Prelude
import Day8
import Data.Array ((//))
import qualified Data.Array as Array
import Control.Monad.Except
import Control.Exception
import qualified Data.Set as Set

allProgramVariants :: Program -> [Program]
allProgramVariants prog = do
  index <- Array.range (Array.bounds prog)

  case prog ! index of 
    Just (NullOp i) -> pure $ prog // [(index, Jump i)] 
    Just (Jump i) -> pure $ prog // [(index, NullOp i)]
    _ -> mempty 

run :: MonadError IOException m => Program -> m Int 
run prog = go $ ProgramState { programCounter = 1, accumulator = 0, instructionsEvaluated = Set.empty }
  where
    go st = case eval prog st of 
      Right newState -> go newState
      Left err@(InfiniteLoop _) -> throwError $ Prelude.userError $ show err
      Left (NormalTermination _) -> pure $ accumulator st

ans :: IO Int
ans = do
  originalProgram <- fileInstructions 
  let 
    programRuns = allProgramVariants originalProgram & mapMaybe (\prog -> case run prog of
      Left _ -> Nothing
      Right acc -> Just acc )

  case viaNonEmpty head programRuns of
    Nothing -> error "Couldn't find a program that terminates"
    Just acc -> pure acc



