{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parser.AST where

import           Data.Text            (Text)
import qualified Data.Text            as T

import           Control.Monad.Except (Except, MonadError (..), handleError)
import           Control.Monad.State  (MonadState (get, put), StateT)
import           Data.Functor         (($>))
import           Numeric.Natural      (Natural)

import           Control.Applicative  (many, optional, (<|>))
import           Lexer.Token          (Token (..), TokenType)
import qualified Lexer.Token          as TK
import           Parser.Error         (ParserError (..), expectedEOF,
                                       unexpectedEOF, unexpectedToken)
import           Pretty               (PrettyPrinter (..), identLines)

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
               | If Exp Statement (Maybe Statement)
               | Null
  deriving (Show)

data Declaration = Declaration Identifier (Maybe Exp)
  deriving (Show)

data Exp = Constant Constant
         | Var Identifier
         | Unary UnaryOperator Exp
         | Binary BinaryOperator Exp Exp
         | Conditional Exp Exp Exp
  deriving (Show)

newtype Constant = CInt Int
  deriving (Show)

data UnaryOperator = Complement
                   | Negate
                   | Not
                   | UnaryAssignmentOperator UnaryAssignmentOperator
  deriving (Show)

data UnaryAssignmentOperator = PreDecrement
                             | PreIncrement
                             | PostDecrement
                             | PostIncrement
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
                              | AddAssign
                              | SubAssign
                              | MulAssign
                              | DivAssign
                              | RemAssign
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
precedence (BinaryAssignmentOperator AddAssign)           = Precedence RightAssociative 1
precedence (BinaryAssignmentOperator SubAssign)           = Precedence RightAssociative 1
precedence (BinaryAssignmentOperator MulAssign)           = Precedence RightAssociative 1
precedence (BinaryAssignmentOperator DivAssign)           = Precedence RightAssociative 1
precedence (BinaryAssignmentOperator RemAssign)           = Precedence RightAssociative 1
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

type ParserState = [Token]
type ParserMonad a = StateT ParserState (Except ParserError) a

class Parser a where
  parse :: ParserMonad a

catchST :: (ParserError -> ParserMonad a) -> ParserMonad a -> ParserMonad a
catchST handler action = get >>= \st -> handleError (\e -> put st >> handler e) action

expectEOF :: ParserMonad ()
expectEOF = get >>= \case
  [] -> return ()
  (t:_) -> throwError $ expectedEOF t

expect :: TokenType -> ParserMonad ()
expect s = get >>= \case
  [] -> throwError $ unexpectedEOF s'
  (t@(Token tt _ _):ts)
         | tt == s -> put ts
         | otherwise -> throwError $ unexpectedToken s' t
  where s' = pretty s

-- <program> ::= <function>
instance Parser Program where
  parse :: ParserMonad Program
  parse = Program <$> parse <* expectEOF

-- <function> ::= "int" <identifier> "(" "void" ")" "{" {<block-item>} "}"
-- <block-item> ::= <statement> | <declaration>
instance Parser FunctionDefinition where
  parse :: ParserMonad FunctionDefinition
  parse = do expect (TK.Keyword TK.Int)
             name <- parse
             mapM_ expect [ TK.OpenParens, TK.Keyword TK.Void, TK.CloseParens ]
             bis <- parseBlock
             return Function { funcName = name
                             , funcBody = bis
                             }

parseBlock :: ParserMonad [BlockItem]
parseBlock = expect TK.OpenBrace *> many parse <* expect TK.CloseBrace

-- <block_item> ::= <statement> | <declaration>
instance Parser BlockItem where
  parse :: ParserMonad BlockItem
  parse = Dec  <$> parse
      <|> Stmt <$> parse

-- <statement> ::= "return" <exp> ";" | <exp> ";" | "if" "(" <exp> ")" <statement> ["else" <statement>] | ";"
instance Parser Statement where
  parse :: ParserMonad Statement
  parse = expect TK.Semicolon $> Null
      <|> expect (TK.Keyword TK.Return) *> (Return <$> parse) <* expect TK.Semicolon
      <|> expect (TK.Keyword TK.If) *> (If <$> (expect TK.OpenParens *> parse <* expect TK.CloseParens) <*> parse <*> optional (expect (TK.Keyword TK.Else) *> parse))
      <|> Expression <$> parse <* expect TK.Semicolon

-- <declaration> ::= "int" <identifier> ["=" <exp>] ";"
instance Parser Declaration where
  parse :: ParserMonad Declaration
  parse = expect (TK.Keyword TK.Int) *> (Declaration <$> parse <*> optional (expect TK.Equals *> parse)) <* expect TK.Semicolon

-- <exp> ::= <assignment-exp>
instance Parser Exp where
  parse :: ParserMonad Exp
  parse = parseAssignmentExp

-- <assignment-exp> ::= <conditional-exp> | <unary-exp> <assignment-op> <assignment-exp>
parseAssignmentExp :: ParserMonad Exp
parseAssignmentExp = flip Binary <$> parseUnaryExp <*> (BinaryAssignmentOperator <$> parse) <*> parseAssignmentExp
                 <|> parseConditionalExp

-- <conditional-exp> ::= <binary-exp> | <binary-exp> "?" <exp> ":" <conditional-exp>
parseConditionalExp :: ParserMonad Exp
parseConditionalExp = Conditional <$> parseBinaryExp <*> (expect TK.QuestionMark *> parse) <*> (expect TK.Colon *> parseConditionalExp)
                  <|> parseBinaryExp

-- Parses binary operator expressions. It uses the operator precedence and associativity to properly parse the expression
-- <binary-exp> ::= <unary-exp> | <unary-exp> <binop> <unary-exp>
parseBinaryExp :: ParserMonad Exp
parseBinaryExp = precedenceClimb 0
  where precedenceClimb :: Natural -> ParserMonad Exp
        precedenceClimb basePrec =
          parseUnaryExp >>= \left -> catchST (const $ return left) (precedenceClimb' basePrec left)
          where precedenceClimb' :: Natural -> Exp -> ParserMonad Exp
                precedenceClimb' minPrec left =
                  catchST (const $ return left) $
                  do st <- get
                     op <- parse
                     let (Precedence opAssociativity opPrecedence) = precedence op
                     if opPrecedence < minPrec then
                       put st >> return left
                     else
                       do let nextPrecedence = case opAssociativity of
                                LeftAssociative  -> opPrecedence + 1
                                RightAssociative -> opPrecedence
                          right <- precedenceClimb nextPrecedence
                          precedenceClimb' minPrec (Binary op left right)

-- <unary-exp> ::= <postfix-exp> | <unary-op> <unary-exp>
-- <unary-op> ::= "-" | "~" | "!" | "++" | "--"
parseUnaryExp :: ParserMonad Exp
parseUnaryExp = expect TK.Decrement  *> (Unary (UnaryAssignmentOperator PreDecrement) <$> parseUnaryExp)
            <|> expect TK.Increment  *> (Unary (UnaryAssignmentOperator PreIncrement) <$> parseUnaryExp)
            <|> expect TK.Minus      *> (Unary Negate                                 <$> parseUnaryExp)
            <|> expect TK.Complement *> (Unary Complement                             <$> parseUnaryExp)
            <|> expect TK.Not        *> (Unary Not                                    <$> parseUnaryExp)
            <|> parsePostfixExp

-- <postfix-exp> ::= <primary-exp> | <postfix-exp> "++" | <postfix-exp> "--"
parsePostfixExp :: ParserMonad Exp
parsePostfixExp = parsePrimaryExp >>= consumePostfix
  where consumePostfix :: Exp -> ParserMonad Exp
        consumePostfix expr = expect TK.Decrement *> consumePostfix (Unary (UnaryAssignmentOperator PostDecrement) expr)
                          <|> expect TK.Increment *> consumePostfix (Unary (UnaryAssignmentOperator PostIncrement) expr)
                          <|> pure expr

-- <primary-exp> ::= <identifier> | <int-constant> | "(" <exp> ")"
parsePrimaryExp :: ParserMonad Exp
parsePrimaryExp = Var      <$> parse
              <|> Constant <$> parse
              <|> expect TK.OpenParens *> parse <* expect TK.CloseParens

-- <binop> ::= "-" | "+" | "*" | "/" | "%" | "&&" | "||"
--           | "==" | "!=" | "<" | "<=" | ">" | ">=" | "="
--           | ">>" | "<<" | "&" | "|" | "^"
instance Parser BinaryOperator where
  parse :: ParserMonad BinaryOperator
  parse = expect TK.Or              $> Or
      <|> expect TK.And             $> And
      <|> expect TK.BitOr           $> BitOr
      <|> expect TK.BitXOR          $> BitXOR
      <|> expect TK.BitAnd          $> BitAnd
      <|> expect TK.EqualsTo        $> EqualsTo
      <|> expect TK.NotEqualsTo     $> NotEqualsTo
      <|> expect TK.Less            $> Less
      <|> expect TK.LessOrEqual     $> LessOrEqual
      <|> expect TK.Greater         $> Greater
      <|> expect TK.GreaterOrEqual  $> GreaterOrEqual
      <|> expect TK.BitShiftLeft    $> BitShiftLeft
      <|> expect TK.BitShiftRight   $> BitShiftRight
      <|> expect TK.Plus            $> Add
      <|> expect TK.Minus           $> Subtract
      <|> expect TK.Asterisk        $> Multiply
      <|> expect TK.ForwardSlash    $> Divide
      <|> expect TK.Percent         $> Remainder
      <|> BinaryAssignmentOperator <$> parse

instance Parser BinaryAssignmentOperator where
  parse :: ParserMonad BinaryAssignmentOperator
  parse = expect TK.Equals              $> Assign
      <|> expect TK.IncAssign           $> AddAssign
      <|> expect TK.DecAssign           $> SubAssign
      <|> expect TK.MulAssign           $> MulAssign
      <|> expect TK.DivAssign           $> DivAssign
      <|> expect TK.ModAssign           $> RemAssign
      <|> expect TK.BitAndAssign        $> BitAndAssign
      <|> expect TK.BitOrAssign         $> BitOrAssign
      <|> expect TK.BitXORAssign        $> BitXORAssign
      <|> expect TK.BitShiftLeftAssign  $> BitShiftLeftAssign
      <|> expect TK.BitShiftRightAssign $> BitShiftRightAssign

-- <constant> ::= Tokens.Constant
instance Parser Constant where
  parse :: ParserMonad Constant
  parse = get >>= \case
    (Token (TK.Constant c) _ _ : ts) -> put ts $> CInt c
    (t : _)                          -> throwError $ unexpectedToken "<constant>" t
    []                               -> throwError $ unexpectedEOF   "<constant>"

-- <identifier> ::= Tokens.Identifier
instance Parser Identifier where
  parse :: ParserMonad Identifier
  parse = get >>= \case
    (Token (TK.Identifier n) _ _ : ts) -> put ts $> n
    (t : _)                            -> throwError $ unexpectedToken "<identifier>" t
    []                                 -> throwError $ unexpectedEOF   "<identifier>"

instance PrettyPrinter Program where
  pretty :: Program -> Text
  pretty (Program f) = "Program\n" <>  f'
    where f' = identLines $ pretty f

instance PrettyPrinter FunctionDefinition where
  pretty :: FunctionDefinition -> Text
  pretty (Function {funcBody=body, funcName=name}) = "Function '" <> name <> "'\n"
                                          <> body'
    where body' = T.concat $ map (identLines . pretty) body

instance PrettyPrinter BlockItem where
  pretty :: BlockItem -> Text
  pretty (Stmt stmt) = pretty stmt
  pretty (Dec dec)   = pretty dec

instance PrettyPrinter Statement where
  pretty :: Statement -> Text
  pretty (Return expr) = ret <> expr' <> ";\n"
    where ret = "return "
          expr' = pretty expr
  pretty (Expression expr) = pretty expr <> ";\n"
  pretty (If cond expThen Nothing) = "if (" <> pretty cond <> ")\n" <> identLines (pretty expThen)
  pretty (If cond expThen (Just expElse)) = "if (" <> pretty cond <> ")\n" <> identLines (pretty expThen) <> "\nelse\n" <> identLines (pretty expElse)
  pretty Null = ";\n"

instance PrettyPrinter Declaration where
  pretty :: Declaration -> Text
  pretty (Declaration name Nothing) = "int " <> name <> ";\n";
  pretty (Declaration name (Just i)) = "int " <> name <> " = " <> pretty i <> ";\n";

instance PrettyPrinter Exp where
  pretty :: Exp -> Text
  pretty (Constant val) = pretty val
  pretty (Var var) = var
  pretty (Unary op@(UnaryAssignmentOperator PostDecrement) expr) = "(" <> pretty expr <> pretty op <> ")"
  pretty (Unary op@(UnaryAssignmentOperator PostIncrement) expr) = "(" <> pretty expr <> pretty op <> ")"
  pretty (Unary op expr) = "(" <> pretty op <> pretty expr <> ")"
  pretty (Binary op exprl exprr) = "(" <> T.intercalate " " [pretty exprl, pretty op, pretty exprr] <> ")"
  pretty (Conditional cond exp1 exp2) = "(" <> pretty cond <> " ? " <> pretty exp1 <> " : " <> pretty exp2 <> ")"

instance PrettyPrinter Constant where
  pretty :: Constant -> Text
  pretty (CInt val) = T.pack $ show val

instance PrettyPrinter UnaryOperator where
  pretty :: UnaryOperator -> Text
  pretty Complement                   = "~"
  pretty Negate                       = "-"
  pretty Not                          = "!"
  pretty (UnaryAssignmentOperator op) = pretty op

instance PrettyPrinter UnaryAssignmentOperator where
  pretty :: UnaryAssignmentOperator -> Text
  pretty PreDecrement  = "--"
  pretty PreIncrement  = "++"
  pretty PostDecrement = "--"
  pretty PostIncrement = "++"

instance PrettyPrinter BinaryOperator where
  pretty :: BinaryOperator -> Text
  pretty Or                                             = "||"
  pretty And                                            = "&&"
  pretty BitOr                                          = "|"
  pretty BitXOR                                         = "^"
  pretty BitAnd                                         = "&"
  pretty EqualsTo                                       = "=="
  pretty NotEqualsTo                                    = "!="
  pretty Less                                           = "<"
  pretty LessOrEqual                                    = "<="
  pretty Greater                                        = ">"
  pretty GreaterOrEqual                                 = ">="
  pretty BitShiftLeft                                   = "<<"
  pretty BitShiftRight                                  = ">>"
  pretty Add                                            = "+"
  pretty Subtract                                       = "-"
  pretty Multiply                                       = "*"
  pretty Divide                                         = "/"
  pretty Remainder                                      = "%"
  pretty (BinaryAssignmentOperator Assign)              = "="
  pretty (BinaryAssignmentOperator AddAssign)           = "+="
  pretty (BinaryAssignmentOperator SubAssign)           = "-="
  pretty (BinaryAssignmentOperator MulAssign)           = "*="
  pretty (BinaryAssignmentOperator DivAssign)           = "/="
  pretty (BinaryAssignmentOperator RemAssign)           = "%="
  pretty (BinaryAssignmentOperator BitAndAssign)        = "&="
  pretty (BinaryAssignmentOperator BitOrAssign)         = "|="
  pretty (BinaryAssignmentOperator BitXORAssign)        = "^="
  pretty (BinaryAssignmentOperator BitShiftLeftAssign)  = "<<="
  pretty (BinaryAssignmentOperator BitShiftRightAssign) = ">>="

