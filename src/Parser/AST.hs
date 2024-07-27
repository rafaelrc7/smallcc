{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.AST where

import qualified Data.Text    as T

import           Lexer.Token  (Token)
import qualified Lexer.Token  as TK
import           Parser.Error

newtype Program = Program FunctionDefinition
  deriving (Show)

data FunctionDefinition = Function { funcName :: T.Text
                                   , funcBody :: Statement
                                   }
  deriving (Show)

newtype Statement = Return Exp
  deriving (Show)

newtype Exp = Constant IntConstant
  deriving (Show)

newtype Identifier = Identifier T.Text
  deriving (Show)

newtype IntConstant = Int Int
  deriving (Show)

class AST a where
  parse :: [Token] -> Either ParserError (a, [Token])

-- <program> ::= <function>
instance AST Program where
  parse :: [Token] -> Either ParserError (Program, [Token])
  parse ts = parse ts >>= \(function, ts') ->
    case ts' of
      []    -> Right (Program function, ts')
      (t:_) -> Left ExpectedEOF {got=t}

-- <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
instance AST FunctionDefinition where
  parse :: [Token] -> Either ParserError (FunctionDefinition, [Token])
  parse (TK.Keyword TK.Int : ts) = parse ts >>= \(Identifier name, ts') ->
    case ts' of
      (TK.OpenParens : TK.Keyword TK.Void : TK.CloseParens : TK.OpenBrace : ts'' ) ->
        parse ts'' >>= \(body, ts''') ->
          case ts''' of
            (TK.CloseBrace : ts'''') -> Right (Function {funcName=name, funcBody=body}, ts'''')
            (t : _) -> Left UnexpectedToken {got=t, expected=TK.CloseBrace}
            [] -> Left UnexpectedEOF {expected=TK.CloseBrace}
      (t : _) -> Left UnexpectedToken {got=t, expected=TK.OpenParens}
      [] -> Left UnexpectedEOF {expected=TK.OpenParens}
  parse (t : _) = Left UnexpectedToken {got=t, expected=TK.Keyword TK.Int}
  parse [] = Left UnexpectedEOF {expected=TK.Keyword TK.Int}


-- <statement> ::= "return" <exp> ";"
instance AST Statement where
  parse :: [Token] -> Either ParserError (Statement, [Token])
  parse (TK.Keyword TK.Return : ts) = parse ts >>= \(expr, ts') ->
    case ts' of
      (TK.Semicolon : ts'') -> Right (Return expr, ts'')
      (t : _) -> Left UnexpectedToken {got=t, expected=TK.Semicolon}
      [] -> Left UnexpectedEOF {expected=TK.Semicolon}
  parse (t : _) = Left UnexpectedToken {got=t, expected=TK.Keyword TK.Return}
  parse [] = Left UnexpectedEOF {expected=TK.Keyword TK.Return}

-- <exp> ::= <int>
instance AST Exp where
  parse :: [Token] -> Either ParserError (Exp, [Token])
  parse ts = parse ts >>= \(constant, ts') -> Right (Constant constant, ts')

-- <identifier> ::= Tokens.Identifier
instance AST Identifier where
  parse :: [Token] -> Either ParserError (Identifier, [Token])
  parse (TK.Identifier v : ts) = Right (Identifier v, ts)
  parse (t : _) = Left UnexpectedToken {got=t, expected=TK.Identifier "identifier"}
  parse [] = Left UnexpectedEOF {expected=TK.Identifier "identifier"}

-- <int> ::= Tokens.Constant
instance AST IntConstant where
  parse :: [Token] -> Either ParserError (IntConstant, [Token])
  parse (TK.Constant v : ts) = Right (Int v, ts)
  parse (t : _) = Left UnexpectedToken {got=t, expected=TK.Constant 0}
  parse [] = Left UnexpectedEOF {expected=TK.Constant 0}

