module Lexer where

import           Lexer.Error          (LexerError (..), LexerErrorType (EOF))
import           Lexer.Scanner        (Buffer, CurrentLexeme (CurrentLexeme),
                                       LexerMonad, LexerState (LexerState),
                                       RemainingBuffer (RemainingBuffer),
                                       nextToken)
import           Lexer.Token          (Location (Location, lexemeColumn, lexemeLine),
                                       Token)

import           Control.Monad.Except (MonadError (throwError), handleError,
                                       runExceptT)
import           Control.Monad.State  (evalState)
import qualified Data.Text            as T
import qualified Data.Text.Lazy.IO    as L

fromFile :: FilePath -> IO Buffer
fromFile = L.readFile

scanUntilEOF :: Buffer -> Either LexerError [Token]
scanUntilEOF buffer = evalState (runExceptT scanUntilEOF') $ LexerState (RemainingBuffer buffer loc) (CurrentLexeme T.empty loc)
  where loc = Location {lexemeColumn=0, lexemeLine=1}
        scanUntilEOF' :: LexerMonad [Token]
        scanUntilEOF' = handleError handler $ (:) <$> nextToken <*> scanUntilEOF'
          where handler :: LexerError -> LexerMonad [Token]
                handler (LexerError EOF _ _) = return []
                handler e                    = throwError e
