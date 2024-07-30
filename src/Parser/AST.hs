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

data Exp = Constant IntConstant
         | Unary UnaryOperator Exp
  deriving (Show)

newtype IntConstant = Int Int
  deriving (Show)

data UnaryOperator = Complement
                   | Negate
  deriving (Show)

newtype Identifier = Identifier T.Text
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
            (t : _) -> Left UnexpectedToken {got=t, expected="}"}
            [] -> Left UnexpectedEOF {expected="}"}
      (t : _) -> Left UnexpectedToken {got=t, expected="("}
      [] -> Left UnexpectedEOF {expected="("}
  parse (t : _) = Left UnexpectedToken {got=t, expected="int"}
  parse [] = Left UnexpectedEOF {expected="int"}


-- <statement> ::= "return" <exp> ";"
instance AST Statement where
  parse :: [Token] -> Either ParserError (Statement, [Token])
  parse (TK.Keyword TK.Return : ts) = parse ts >>= \(expr, ts') ->
    case ts' of
      (TK.Semicolon : ts'') -> Right (Return expr, ts'')
      (t : _)               -> Left UnexpectedToken {got=t, expected=";"}
      []                    -> Left UnexpectedEOF {expected=";"}
  parse (t : _) = Left UnexpectedToken {got=t, expected="return"}
  parse [] = Left UnexpectedEOF {expected="return"}

-- <exp> ::= <int> | <unop> <exp> | "(" <exp> ")"
-- <unop> ::= "-" | "~"
instance AST Exp where
  parse :: [Token] -> Either ParserError (Exp, [Token])
  parse (TK.OpenParens : ts) = parse ts >>= \(expr, ts') ->
    case ts' of
      (TK.CloseParens : ts'') -> Right (expr, ts'')
      (t : _)                 -> Left UnexpectedToken {got=t, expected=")" }
      []                      -> Left UnexpectedEOF {expected=")"}
  parse (TK.Minus : ts) = parse ts >>= \(expr, ts') -> Right (Unary Negate expr, ts')
  parse (TK.Complement : ts) = parse ts >>= \(expr, ts') -> Right (Unary Complement expr, ts')
  parse ts = parse ts >>= \(constant, ts') -> Right (Constant constant, ts')

-- <int> ::= Tokens.Constant
instance AST IntConstant where
  parse :: [Token] -> Either ParserError (IntConstant, [Token])
  parse (TK.Constant v : ts) = Right (Int v, ts)
  parse (t : _)              = Left UnexpectedToken {got=t, expected="<int>"}
  parse []                   = Left UnexpectedEOF {expected="<int>"}

-- <identifier> ::= Tokens.Identifier
instance AST Identifier where
  parse :: [Token] -> Either ParserError (Identifier, [Token])
  parse (TK.Identifier v : ts) = Right (Identifier v, ts)
  parse (t : _) = Left UnexpectedToken {got=t, expected="<identifier>"}
  parse [] = Left UnexpectedEOF {expected="<identifier>"}

