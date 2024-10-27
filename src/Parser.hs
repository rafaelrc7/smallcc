module Parser where

import           Control.Monad.Except (runExcept)
import           Control.Monad.State  (evalStateT)
import           Lexer.Token          (Token)
import           Parser.AST           (Parser (parse), Program)
import           Parser.Error         (ParserError)

parseProgram :: [Token] -> Either ParserError Program
parseProgram = runExcept . evalStateT parse

