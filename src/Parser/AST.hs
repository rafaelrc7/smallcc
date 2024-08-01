{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parser.AST where

import           Data.Text    (Text)

import           Lexer.Token  (Token)
import qualified Lexer.Token  as TK
import           Parser.Error

newtype Program = Program FunctionDefinition
  deriving (Show)

data FunctionDefinition = Function { funcName :: Identifier
                                   , funcBody :: Statement
                                   }
  deriving (Show)

newtype Statement = Return Exp
  deriving (Show)

data Exp = Constant Constant
         | Unary UnaryOperator Exp
         | Binary BinaryOperator Exp Exp
  deriving (Show)

newtype Constant = CInt Int
  deriving (Show)

data UnaryOperator = Complement
                   | Negate
  deriving (Show)

data BinaryOperator = Add
                    | Subtract
                    | Multiply
                    | Divide
                    | Remainder
  deriving (Show)

type Identifier = Text

precedence :: BinaryOperator -> Int
precedence Add       = 45
precedence Subtract  = 45
precedence Multiply  = 50
precedence Divide    = 50
precedence Remainder = 50

class Parser a where
  parse :: [Token] -> Either ParserError (a, [Token])

-- <program> ::= <function>
instance Parser Program where
  parse :: [Token] -> Either ParserError (Program, [Token])
  parse ts = parse ts >>= \(function, ts') ->
    case ts' of
      []    -> Right (Program function, ts')
      (t:_) -> Left ExpectedEOF {got=t}

-- <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
instance Parser FunctionDefinition where
  parse :: [Token] -> Either ParserError (FunctionDefinition, [Token])
  parse (TK.Keyword TK.Int : ts) = parse ts >>= \(name, ts') ->
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
instance Parser Statement where
  parse :: [Token] -> Either ParserError (Statement, [Token])
  parse (TK.Keyword TK.Return : ts) = parse ts >>= \(expr, ts') ->
    case ts' of
      (TK.Semicolon : ts'') -> Right (Return expr, ts'')
      (t : _)               -> Left UnexpectedToken {got=t, expected=";"}
      []                    -> Left UnexpectedEOF {expected=";"}
  parse (t : _) = Left UnexpectedToken {got=t, expected="return"}
  parse [] = Left UnexpectedEOF {expected="return"}

-- <exp> ::= <factor> | <exp> <binop> <exp>
-- <factor> ::= <int> | <unop> <exp> | "(" <exp> ")"
-- <unop> ::= "-" | "~"
-- <binop> ::= "-" | "+" | "*" | "/" | "%"
instance Parser Exp where
  parse :: [Token] -> Either ParserError (Exp, [Token])
  parse = parseExp 0

parseExp :: Int -> [Token] -> Either ParserError (Exp, [Token])
parseExp minPrecedence ts =
  parseFactor ts >>= \(left, ts') ->
  case parse ts' :: Either ParserError (BinaryOperator, [Token]) of
    Left _  -> Right (left, ts')
    Right _ -> parseExp' minPrecedence ts' left
  where parseExp' :: Int -> [Token] -> Exp -> Either ParserError (Exp, [Token])
        parseExp' minPrec ts' left = case parse ts' :: Either ParserError (BinaryOperator, [Token]) of
          Left _ -> Right (left, ts')
          Right (op, ts'')
              | opPrecedence >= minPrec ->
                  parseExp (opPrecedence + 1) ts'' >>= \(right, ts''') ->
                  parseExp' minPrec ts''' (Binary op left right)
              | otherwise -> Right (left, ts')
            where opPrecedence = precedence op

parseFactor :: [Token] -> Either ParserError (Exp, [Token])
parseFactor (TK.Minus : ts) = parseFactor ts >>= \(expr, ts') -> Right (Unary Negate expr, ts')
parseFactor (TK.Complement : ts) = parseFactor ts >>= \(expr, ts') -> Right (Unary Complement expr, ts')
parseFactor (TK.OpenParens : ts) = parse ts >>= \(expr, ts') ->
  case ts' of
    (TK.CloseParens : ts'') -> Right (expr, ts'')
    (t : _)                 -> Left UnexpectedToken {got=t, expected=")" }
    []                      -> Left UnexpectedEOF {expected=")"}
parseFactor ts = parse ts >>= \(constant, ts') -> Right (Constant constant, ts')

instance Parser BinaryOperator where
  parse :: [Token] -> Either ParserError (BinaryOperator, [Token])
  parse (TK.Minus : ts) = Right (Subtract, ts)
  parse (TK.Plus : ts) = Right (Add, ts)
  parse (TK.Asterisk : ts) = Right (Multiply, ts)
  parse (TK.ForwardSlash : ts) = Right (Divide, ts)
  parse (TK.Percent : ts) = Right (Remainder, ts)
  parse (t : _) = Left UnexpectedToken {got=t, expected="<binop>"}
  parse [] = Left UnexpectedEOF {expected="<binop>"}

-- <int> ::= Tokens.Constant
instance Parser Constant where
  parse :: [Token] -> Either ParserError (Constant, [Token])
  parse (TK.Constant v : ts) = Right (CInt v, ts)
  parse (t : _)              = Left UnexpectedToken {got=t, expected="<int>"}
  parse []                   = Left UnexpectedEOF {expected="<int>"}

-- <identifier> ::= Tokens.Identifier
instance Parser Identifier where
  parse :: [Token] -> Either ParserError (Identifier, [Token])
  parse (TK.Identifier v : ts) = Right (v, ts)
  parse (t : _) = Left UnexpectedToken {got=t, expected="<identifier>"}
  parse [] = Left UnexpectedEOF {expected="<identifier>"}

