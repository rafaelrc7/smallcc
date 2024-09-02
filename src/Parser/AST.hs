{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parser.AST where

import           Data.Text       (Text)

import           Lexer.Token     (Token)
import qualified Lexer.Token     as TK
import           Numeric.Natural (Natural)
import           Parser.Error

newtype Program = Program FunctionDefinition
  deriving (Show)

data FunctionDefinition = Function { funcName :: Identifier
                                   , funcBody :: [BlockItem]
                                   }
  deriving (Show)

data BlockItem = Stmt Statement
               | Dec Declaration
  deriving (Show)

data Statement = Return Exp
               | Expression Exp
               | Null
  deriving (Show)

data Declaration = Declaration Identifier (Maybe Exp)
  deriving (Show)

data Exp = Constant Constant
         | Var Identifier
         | Unary UnaryOperator Exp
         | Binary BinaryOperator Exp Exp
         | Assignment Exp Exp
         | PreAssignment UnaryAssignmentOperator Exp
         | PostAssignment UnaryAssignmentOperator Exp
  deriving (Show)

newtype Constant = CInt Int
  deriving (Show)

data UnaryOperator = Complement
                   | Negate
                   | Not
                   | UnaryAssignmentOperator UnaryAssignmentOperator
  deriving (Show)

data UnaryAssignmentOperator = Decrement
                             | Increment
  deriving (Show)

data BinaryOperator = Add
                    | Subtract
                    | Multiply
                    | Divide
                    | Remainder
                    | BitAnd
                    | BitOr
                    | BitXOR
                    | BitShiftLeft
                    | BitShiftRight
                    | And
                    | Or
                    | EqualsTo
                    | NotEqualsTo
                    | Less
                    | LessOrEqual
                    | Greater
                    | GreaterOrEqual
                    | BinaryAssignmentOperator BinaryAssignmentOperator
  deriving (Show)

data BinaryAssignmentOperator = Assign
                              | IncAssign
                              | DecAssign
                              | MulAssign
                              | DivAssign
                              | ModAssign
                              | BitAndAssign
                              | BitOrAssign
                              | BitXORAssign
                              | BitShiftLeftAssign
                              | BitShiftRightAssign
  deriving (Show)

type Identifier = Text

data Associativity = LeftAssociative | RightAssociative

data Precedence = Precedence Associativity Natural

-- https://en.cppreference.com/w/c/language/operator_precedence
precedence :: BinaryOperator -> Precedence
precedence (BinaryAssignmentOperator Assign)              = Precedence RightAssociative 1
precedence (BinaryAssignmentOperator IncAssign)           = Precedence RightAssociative 1
precedence (BinaryAssignmentOperator DecAssign)           = Precedence RightAssociative 1
precedence (BinaryAssignmentOperator MulAssign)           = Precedence RightAssociative 1
precedence (BinaryAssignmentOperator DivAssign)           = Precedence RightAssociative 1
precedence (BinaryAssignmentOperator ModAssign)           = Precedence RightAssociative 1
precedence (BinaryAssignmentOperator BitAndAssign)        = Precedence RightAssociative 1
precedence (BinaryAssignmentOperator BitOrAssign)         = Precedence RightAssociative 1
precedence (BinaryAssignmentOperator BitXORAssign)        = Precedence RightAssociative 1
precedence (BinaryAssignmentOperator BitShiftLeftAssign)  = Precedence RightAssociative 1
precedence (BinaryAssignmentOperator BitShiftRightAssign) = Precedence RightAssociative 1
precedence Or                                             = Precedence LeftAssociative  5
precedence And                                            = Precedence LeftAssociative  10
precedence BitOr                                          = Precedence LeftAssociative  15
precedence BitXOR                                         = Precedence LeftAssociative  20
precedence BitAnd                                         = Precedence LeftAssociative  25
precedence EqualsTo                                       = Precedence LeftAssociative  30
precedence NotEqualsTo                                    = Precedence LeftAssociative  30
precedence Less                                           = Precedence LeftAssociative  35
precedence LessOrEqual                                    = Precedence LeftAssociative  35
precedence Greater                                        = Precedence LeftAssociative  35
precedence GreaterOrEqual                                 = Precedence LeftAssociative  35
precedence BitShiftLeft                                   = Precedence LeftAssociative  40
precedence BitShiftRight                                  = Precedence LeftAssociative  40
precedence Add                                            = Precedence LeftAssociative  45
precedence Subtract                                       = Precedence LeftAssociative  45
precedence Multiply                                       = Precedence LeftAssociative  50
precedence Divide                                         = Precedence LeftAssociative  50
precedence Remainder                                      = Precedence LeftAssociative  50

class Parser a where
  parse :: [Token] -> Either ParserError (a, [Token])

-- <program> ::= <function>
instance Parser Program where
  parse :: [Token] -> Either ParserError (Program, [Token])
  parse ts = parse ts >>= \case
      (func, ts'@[])  -> Right (Program func, ts')
      (_,    t : _)     -> Left ExpectedEOF {got=t}

-- <function> ::= "int" <identifier> "(" "void" ")" "{" {<block-item>} "}"
-- <block-item> ::= <statement> | <declaration>
instance Parser FunctionDefinition where
  parse :: [Token] -> Either ParserError (FunctionDefinition, [Token])
  parse (TK.Keyword TK.Int : ts) = parse ts >>= \case
    (name, TK.OpenParens : TK.Keyword TK.Void : TK.CloseParens : TK.OpenBrace : ts' ) ->
        case parseBlockItens ts' of
          (body, TK.CloseBrace : ts'') -> Right (Function {funcName=name, funcBody=body}, ts'')
          (_,    t : _)                  -> Left UnexpectedToken {got=t, expected="}"}
          (_,    [])                     -> Left UnexpectedEOF {expected="}"}
    (_, t : _) -> Left UnexpectedToken {got=t, expected="("}
    (_, []) -> Left UnexpectedEOF {expected="("}
  parse (t : _) = Left UnexpectedToken {got=t, expected="int"}
  parse [] = Left UnexpectedEOF {expected="int"}

-- <statement> ::= "return" <exp> ";"
instance Parser Statement where
  parse :: [Token] -> Either ParserError (Statement, [Token])
  parse (TK.Semicolon : ts) = Right (Null, ts)
  parse (TK.Keyword TK.Return : ts) = parse ts >>= \case
      (expr, TK.Semicolon : ts') -> Right (Return expr, ts')
      (_,    t : _)               -> Left UnexpectedToken {got=t, expected=";"}
      (_,    [])                  -> Left UnexpectedEOF {expected=";"}
  parse ts = parse ts >>= \case
    (expr, TK.Semicolon : ts') -> Right (Expression expr, ts')
    (_,    t : _)               -> Left UnexpectedToken {got=t, expected=";"}
    (_,    [])                  -> Left UnexpectedEOF {expected=";"}

-- <declaration> ::= "int" <identifier> ["=" <exp>] ";"
instance Parser Declaration where
  parse :: [Token] -> Either ParserError (Declaration, [Token])
  parse (TK.Keyword TK.Int : ts) = parse ts >>= \case
    (name, TK.Semicolon : ts') -> Right (Declaration name Nothing, ts')
    (name, TK.Equals : ts') -> parse ts' >>= \case
      (expr, TK.Semicolon : ts'') -> Right (Declaration name (Just expr), ts'')
      (_, t : _) -> Left UnexpectedToken {got=t, expected=";"}
      (_, []) -> Left UnexpectedEOF {expected=";"}
    (_,     t : _) -> Left UnexpectedToken {got=t, expected="; | <exp>;"}
    (_,     []) -> Left UnexpectedEOF {expected="; | <exp>;"}
  parse (t : _) = Left UnexpectedToken {got=t, expected="int"}
  parse [] = Left UnexpectedEOF {expected="int"}

-- <block_item> ::= <statement> | <declaration>
instance Parser BlockItem where
  parse :: [Token] -> Either ParserError (BlockItem, [Token])
  parse ts@(TK.Keyword TK.Int : _) = parse ts >>= \(dec, ts') -> Right (Dec dec, ts')
  parse ts = parse ts >>= \(stmt, ts') -> Right (Stmt stmt, ts')

-- {<block_item>} ::= <block_item>*
parseBlockItens :: [Token] -> ([BlockItem], [Token])
parseBlockItens ts =
  case parse ts of
    Right (blockItem, ts') -> (blockItem : bis, ts'')
      where (bis, ts'') = parseBlockItens ts'
    Left _ -> ([], ts)

-- <exp> ::= <factor> | <exp> <binop> <exp>
instance Parser Exp where
  parse :: [Token] -> Either ParserError (Exp, [Token])
  parse = precedenceClimb 0

precedenceClimb :: Natural -> [Token] -> Either ParserError (Exp, [Token])
precedenceClimb minPrecedence ts =
  parseFactor ts >>= \(left, ts') ->
    case parse ts' :: Either ParserError (BinaryOperator, [Token]) of
      Left _  -> Right (left, ts')
      Right _ -> precedenceClimb' minPrecedence ts' left
  where precedenceClimb' :: Natural -> [Token] -> Exp -> Either ParserError (Exp, [Token])
        precedenceClimb' minPrec ts' left = case parse ts' of
          Left _ -> Right (left, ts')
          Right (op, ts'') ->
            if opPrecedence < minPrec then
              Right (left, ts')
            else do let nextPrecedence = case opAssociativity of
                         LeftAssociative  -> opPrecedence + 1
                         RightAssociative -> opPrecedence
                    (right, ts''') <- precedenceClimb nextPrecedence ts''
                    let expr = case op of
                         BinaryAssignmentOperator Assign              -> Assignment left right
                         BinaryAssignmentOperator IncAssign           -> Assignment left (Binary Add left right)
                         BinaryAssignmentOperator DecAssign           -> Assignment left (Binary Subtract left right)
                         BinaryAssignmentOperator MulAssign           -> Assignment left (Binary Multiply left right)
                         BinaryAssignmentOperator DivAssign           -> Assignment left (Binary Divide left right)
                         BinaryAssignmentOperator ModAssign           -> Assignment left (Binary Remainder left right)
                         BinaryAssignmentOperator BitAndAssign        -> Assignment left (Binary BitAnd left right)
                         BinaryAssignmentOperator BitOrAssign         -> Assignment left (Binary BitOr left right)
                         BinaryAssignmentOperator BitXORAssign        -> Assignment left (Binary BitXOR left right)
                         BinaryAssignmentOperator BitShiftLeftAssign  -> Assignment left (Binary BitShiftLeft left right)
                         BinaryAssignmentOperator BitShiftRightAssign -> Assignment left (Binary BitShiftRight left right)
                         _                                            -> Binary op left right
                    precedenceClimb' minPrec ts''' expr
              where
                (Precedence opAssociativity opPrecedence) = precedence op

-- <factor> ::= <int> | <unop> <exp> | <exp> <unop> | "(" <exp> ")"
-- <unop> ::= "-" | "~" | "!" | "++" | "--"
parseFactor :: [Token] -> Either ParserError (Exp, [Token])
parseFactor tokens = parseFactor' tokens >>= \(expr, ts') -> consumePostfix ts' expr
  where parseFactor' :: [Token] -> Either ParserError (Exp, [Token])
        parseFactor' (TK.Decrement : ts) = parseFactor ts >>= \(expr, ts') -> Right (PreAssignment Decrement expr, ts')
        parseFactor' (TK.Increment : ts) = parseFactor ts >>= \(expr, ts') -> Right (PreAssignment Increment expr, ts')
        parseFactor' (TK.Minus : ts) = parseFactor ts >>= \(expr, ts') -> Right (Unary Negate expr, ts')
        parseFactor' (TK.Complement : ts) = parseFactor ts >>= \(expr, ts') -> Right (Unary Complement expr, ts')
        parseFactor' (TK.Not : ts) = parseFactor ts >>= \(expr, ts') -> Right (Unary Not expr, ts')
        parseFactor' (TK.OpenParens : ts) = parse ts >>= \case
            (expr, TK.CloseParens : ts') -> Right (expr, ts')
            (_,    t : _)                 -> Left UnexpectedToken {got=t, expected=")" }
            (_,    [])                    -> Left UnexpectedEOF {expected=")"}
        parseFactor' ts@(TK.Constant _ : _) = parse ts >>= \(constant, ts') -> Right (Constant constant, ts')
        parseFactor' ts@(TK.Identifier _ : _) = parse ts >>= \(var, ts') -> Right (Var var, ts')
        parseFactor' (t : _) = Left $ UnexpectedToken {got=t, expected="<exp>"}
        parseFactor' [] = Left $ UnexpectedEOF {expected="<exp>"}

        consumePostfix :: [Token] -> Exp -> Either ParserError (Exp, [Token])
        consumePostfix (TK.Decrement : ts) expr = consumePostfix ts $ PostAssignment Decrement expr
        consumePostfix (TK.Increment : ts) expr = consumePostfix ts $ PostAssignment Increment expr
        consumePostfix ts expr = Right (expr, ts)

-- <binop> ::= "-" | "+" | "*" | "/" | "%" | "&&" | "||"
--           | "==" | "!=" | "<" | "<=" | ">" | ">=" | "="
--           | ">>" | "<<" | "&" | "|" | "^"
instance Parser BinaryOperator where
  parse :: [Token] -> Either ParserError (BinaryOperator, [Token])
  parse (TK.Or : ts) = Right (Or, ts)
  parse (TK.And : ts) = Right (And, ts)
  parse (TK.BitOr : ts) = Right (BitOr, ts)
  parse (TK.BitXOR : ts) = Right (BitXOR, ts)
  parse (TK.BitAnd : ts) = Right (BitAnd, ts)
  parse (TK.EqualsTo : ts) = Right (EqualsTo, ts)
  parse (TK.NotEqualsTo : ts) = Right (NotEqualsTo, ts)
  parse (TK.Less : ts) = Right (Less, ts)
  parse (TK.LessOrEqual : ts) = Right (LessOrEqual, ts)
  parse (TK.Greater : ts) = Right (Greater, ts)
  parse (TK.GreaterOrEqual : ts) = Right (GreaterOrEqual, ts)
  parse (TK.BitShiftLeft : ts) = Right (BitShiftLeft, ts)
  parse (TK.BitShiftRight : ts) = Right (BitShiftRight, ts)
  parse (TK.Plus : ts) = Right (Add, ts)
  parse (TK.Minus : ts) = Right (Subtract, ts)
  parse (TK.Asterisk : ts) = Right (Multiply, ts)
  parse (TK.ForwardSlash : ts) = Right (Divide, ts)
  parse (TK.Percent : ts) = Right (Remainder, ts)
  parse ts = parse ts >>= \(op, ts') -> Right (BinaryAssignmentOperator op, ts')

instance Parser BinaryAssignmentOperator where
  parse :: [Token] -> Either ParserError (BinaryAssignmentOperator, [Token])
  parse (TK.Equals : ts) = Right (Assign, ts)
  parse (TK.IncAssign : ts) = Right (IncAssign, ts)
  parse (TK.DecAssign : ts) = Right (DecAssign, ts)
  parse (TK.MulAssign : ts) = Right (MulAssign, ts)
  parse (TK.DivAssign : ts) = Right (DivAssign, ts)
  parse (TK.ModAssign : ts) = Right (ModAssign, ts)
  parse (TK.BitAndAssign : ts) = Right (BitAndAssign, ts)
  parse (TK.BitOrAssign : ts) = Right (BitOrAssign, ts)
  parse (TK.BitXORAssign : ts) = Right (BitXORAssign, ts)
  parse (TK.BitShiftLeftAssign : ts) = Right (BitShiftLeftAssign, ts)
  parse (TK.BitShiftRightAssign : ts) = Right (BitShiftRightAssign, ts)
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

