module Parser where

import           Control.Monad.Except (runExcept)
import           Control.Monad.State  (evalStateT)
import           Lexer.Token          (Token)
import           Parser.AST           (Program)
import           Parser.Error         (ParserError)
import           Parser.ParserMonad   (Parser (parse), ParserPhase)

parseProgram :: [Token] -> Either ParserError (Program ParserPhase)
parseProgram = runExcept . evalStateT parse
