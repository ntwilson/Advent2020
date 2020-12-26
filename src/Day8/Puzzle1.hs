module Day8.Puzzle1 where

import Relude
import qualified Prelude
import Day8
import qualified Data.Set as Set
import Control.Monad.Except (MonadError, throwError)
import Control.Exception (IOException)

run :: MonadError IOException m => Program -> m Int 
run prog = go $ ProgramState { programCounter = 1, accumulator = 0, instructionsEvaluated = Set.empty }
  where
    go st = case eval prog st of 
      Right newState -> go newState
      Left err@(NormalTermination _) -> throwError $ Prelude.userError $ show err
      Left (InfiniteLoop _) -> pure $ accumulator st

ans :: IO Int
ans = run =<< fileInstructions 
