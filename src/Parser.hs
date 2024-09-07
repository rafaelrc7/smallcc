module Parser where

import           Control.Monad.Except (runExceptT)
import           Control.Monad.State  (evalState)
import           Lexer.Token          (Token)
import           Location             (Location (Location))
import           Parser.AST           (Parser (parse), Program)
import           Parser.Error         (ParserError)

parseProgram :: [Token] -> Either ParserError Program
parseProgram ts = evalState (runExceptT parse) (ts, Location 1 1 Nothing)

