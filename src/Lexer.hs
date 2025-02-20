module Lexer where

import           Control.Monad.Except (MonadError (throwError), handleError,
                                       runExceptT)
import           Control.Monad.State  (evalState)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Lazy.IO    as L
import           Lexer.Error          (LexerError (..),
                                       LexerErrorType (ReachedEOF))
import           Lexer.Scanner        (Buffer, CurrentLexeme (CurrentLexeme),
                                       LexerMonad, LexerState (LexerState),
                                       RemainingBuffer (RemainingBuffer),
                                       nextToken)
import           Lexer.Token          (Token)
import           Location

fromFile :: FilePath -> IO Buffer
fromFile = L.readFile

scanUntilEOF :: Text -> Buffer -> Either LexerError [Token]
scanUntilEOF bufferName buffer = evalState (runExceptT scanUntilEOF') $ LexerState (RemainingBuffer buffer loc) (CurrentLexeme T.empty loc)
  where
    loc = Location 1 1 (Just bufferName)
    scanUntilEOF' :: LexerMonad [Token]
    scanUntilEOF' = handleError handler $ (:) <$> nextToken <*> scanUntilEOF'
      where
        handler :: LexerError -> LexerMonad [Token]
        handler (LexerError ReachedEOF _ _) = return []
        handler e                           = throwError e
