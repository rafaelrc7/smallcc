module Parser where

import           Lexer.Token
import           Parser.AST
import           Parser.Error

parseProgram :: [Token] -> Either ParserError Program
parseProgram ts = parse ts >>= \(program, _) -> Right program

