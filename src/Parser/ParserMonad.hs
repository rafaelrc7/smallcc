{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parser.ParserMonad where

import           Control.Applicative  (Alternative (many, (<|>)), optional)
import           Control.Monad.Except (Except, MonadError (throwError),
                                       handleError)
import           Control.Monad.State  (MonadState (get, put), StateT)
import           Data.Functor         (($>))
import           Lexer.Token          (Token (..), TokenType)
import qualified Lexer.Token          as TK
import           Numeric.Natural      (Natural)
import           Parser.AST           (AssignmentOperator (..),
                                       Associativity (LeftAssociative, RightAssociative),
                                       BinaryOperator (..), Block (Block),
                                       BlockItem (..), Constant (..),
                                       Declaration (..), Exp (..),
                                       FunctionDefinition (..), Identifier,
                                       Precedence (Precedence), Program (..),
                                       Statement (..),
                                       UnaryAssignmentOperator (PostDecrement, PostIncrement, PreDecrement, PreIncrement),
                                       UnaryOperator (Complement, Negate, Not, UnaryAssignmentOperator),
                                       precedence)
import           Parser.Error         (ParserError, expectedEOF, unexpectedEOF,
                                       unexpectedToken)
import           Pretty               (PrettyPrinter (pretty))

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

-- <function> ::= "int" <identifier> "(" "void" ")" <block>
instance Parser FunctionDefinition where
  parse :: ParserMonad FunctionDefinition
  parse = do expect (TK.Keyword TK.Int)
             name <- parse
             mapM_ expect [ TK.OpenParens, TK.Keyword TK.Void, TK.CloseParens ]
             Function name <$> parse

-- <block> ::= "{" {<block-item>} "}"
instance Parser Block where
  parse :: ParserMonad Block
  parse = expect TK.OpenBrace *> (Block <$> many parse) <* expect TK.CloseBrace

-- <block_item> ::= <statement> | <declaration>
instance Parser BlockItem where
  parse :: ParserMonad BlockItem
  parse = Dec  <$> parse
      <|> Stmt <$> parse

-- <statement> ::= "return" <exp> ";" | <exp> ";" | "if" "(" <exp> ")" <statement> ["else" <statement>] | "goto" <identifier> | <identifier> ":" | <block> | ";"
instance Parser Statement where
  parse :: ParserMonad Statement
  parse = expect TK.Semicolon $> Null
      <|> expect (TK.Keyword TK.Return) *> (Return <$> parse) <* expect TK.Semicolon
      <|> expect (TK.Keyword TK.If) *> (If <$> (expect TK.OpenParens *> parse <* expect TK.CloseParens) <*> parse <*> optional (expect (TK.Keyword TK.Else) *> parse))
      <|> expect (TK.Keyword TK.Goto) *> (Goto <$> parse) <* expect TK.Semicolon
      <|> (Label <$> parse) <* expect TK.Colon
      <|> Compound <$> parse
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
parseAssignmentExp = Assignment <$> parseConditionalExp <*> parse <*> parseAssignmentExp
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

instance Parser AssignmentOperator where
  parse :: ParserMonad AssignmentOperator
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

