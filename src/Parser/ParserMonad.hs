{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

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
                                       ForInit (InitDecl, InitExp),
                                       FunctionDeclaration (FunctionDeclaration),
                                       Identifier, Label (..),
                                       Precedence (Precedence), Program (..),
                                       Statement (..),
                                       UnaryAssignmentOperator (PostDecrement, PostIncrement, PreDecrement, PreIncrement),
                                       UnaryOperator (Complement, Negate, Not, UnaryAssignmentOperator),
                                       UnlabeledStatement (..),
                                       VarDeclaration (VarDeclaration),
                                       XAssignment, XBinary, XBreak, XCase,
                                       XCaseV, XCompound, XConditional,
                                       XConstant, XContinue, XDefault, XDoWhile,
                                       XExpression, XFor, XFunDecl,
                                       XFunctionCall, XGoto, XIf, XLabel, XNull,
                                       XReturn, XSwitch, XUnary, XVar, XVarDecl,
                                       XWhile, precedence)
import           Parser.Error         (ParserError, expectedEOF, unexpectedEOF,
                                       unexpectedToken)
import           Pretty               (PrettyPrinter (pretty))


-- ParserPhase specific decorators --

data ParserPhase

type instance XExpression   ParserPhase = ()
type instance XCompound     ParserPhase = ()
type instance XIf           ParserPhase = ()
type instance XSwitch       ParserPhase = ()
type instance XWhile        ParserPhase = ()
type instance XDoWhile      ParserPhase = ()
type instance XFor          ParserPhase = ()
type instance XGoto         ParserPhase = ()
type instance XContinue     ParserPhase = ()
type instance XBreak        ParserPhase = ()
type instance XReturn       ParserPhase = ()
type instance XNull         ParserPhase = ()
type instance XLabel        ParserPhase = ()
type instance XCase         ParserPhase = ()
type instance XCaseV        ParserPhase = Exp ParserPhase
type instance XDefault      ParserPhase = ()
type instance XConstant     ParserPhase = ()
type instance XVar          ParserPhase = ()
type instance XUnary        ParserPhase = ()
type instance XBinary       ParserPhase = ()
type instance XAssignment   ParserPhase = ()
type instance XConditional  ParserPhase = ()
type instance XFunDecl      ParserPhase = ()
type instance XVarDecl      ParserPhase = ()
type instance XFunctionCall ParserPhase = ()


-- ParserMonad --

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


-- Parsing Grammar --

-- <program> ::= {<function>}
instance Parser (Program ParserPhase) where
  parse :: ParserMonad (Program ParserPhase)
  parse = Program <$> many parse <* expectEOF

-- <declaration> ::= <function-declaration> | <var-declaration>
instance Parser (Declaration ParserPhase) where
  parse :: ParserMonad (Declaration ParserPhase)
  parse = VarDecl <$> parse
      <|> FunDecl <$> parse

-- <function-declaration> ::= "int" <identifier> "(" <param-list> ")" (<block> | ";")
-- <param-lsit> :: = "void" | "int" <identifier> {"," "int" <identifier>}
instance Parser (FunctionDeclaration ParserPhase) where
  parse :: ParserMonad (FunctionDeclaration ParserPhase)
  parse = do expect (TK.Keyword TK.Int)
             name <- parse
             expect TK.OpenParens
             params <- (:) <$> (expect (TK.Keyword TK.Int) *> parse) <*> many (expect TK.Comma *> expect (TK.Keyword TK.Int) *> parse)
                   <|> expect (TK.Keyword TK.Void) $> []
             expect TK.CloseParens
             body <- expect TK.Semicolon $> Nothing
                 <|> Just <$> parse
             return $ FunctionDeclaration () name params body

-- <var-declaration> ::= "int" <identifier> ["=" <exp>] ";"
instance Parser (VarDeclaration ParserPhase) where
  parse :: ParserMonad (VarDeclaration ParserPhase)
  parse = expect (TK.Keyword TK.Int) *> (VarDeclaration () <$> parse <*> optional (expect TK.Equals *> parse)) <* expect TK.Semicolon

-- <block> ::= "{" {<block-item>} "}"
instance Parser (Block ParserPhase) where
  parse :: ParserMonad (Block ParserPhase)
  parse = expect TK.OpenBrace *> (Block <$> many parse) <* expect TK.CloseBrace

-- <block_item> ::= <statement> | <declaration>
instance Parser (BlockItem ParserPhase) where
  parse :: ParserMonad (BlockItem ParserPhase)
  parse = BlockDeclaration <$> parse
      <|> BlockStatement   <$> parse

-- <statement> ::= <label> ":" <statement> | <unlabeled-statement>
instance Parser (Statement ParserPhase) where
  parse :: ParserMonad (Statement ParserPhase)
  parse = LabeledStatement <$> parse <*> parse
      <|> UnlabeledStatement <$> parse

-- <unlabeled-statement> ::= "return" <exp> ";"
--                       | <exp> ";"
--                       | "if" "(" <exp> ")" <statement> ["else" <statement>]
--                       | "goto" <identifier>
--                       | <block>
--                       | "break" ";"
--                       | "continue" ";"
--                       | "while" "(" <exp> ")" <statement>
--                       | "do" <statement> "while" "(" <exp> ")" ";"
--                       | "for" "(" <for-init> [<exp>] ";" [<exp>] ")" <statement>
--                       | ";"
instance Parser (UnlabeledStatement ParserPhase) where
  parse :: ParserMonad (UnlabeledStatement ParserPhase)
  parse = expect TK.Semicolon $> Null ()
      <|> expect (TK.Keyword TK.Return) *> (Return () <$> parse) <* expect TK.Semicolon
      <|> expect (TK.Keyword TK.If) *> (If () <$> (expect TK.OpenParens *> parse <* expect TK.CloseParens) <*> parse <*> optional (expect (TK.Keyword TK.Else) *> parse))
      <|> expect (TK.Keyword TK.Goto) *> (Goto () <$> parse) <* expect TK.Semicolon
      <|> expect (TK.Keyword TK.Break) $> Break () <* expect TK.Semicolon
      <|> expect (TK.Keyword TK.Continue) $> Continue () <* expect TK.Semicolon
      <|> expect (TK.Keyword TK.While) *> (While () <$> (expect TK.OpenParens *> parse <* expect TK.CloseParens) <*> parse)
      <|> expect (TK.Keyword TK.Do) *> (DoWhile () <$> parse <*> (expect (TK.Keyword TK.While) *> expect TK.OpenParens *> parse <* expect TK.CloseParens)) <* expect TK.Semicolon
      <|> expect (TK.Keyword TK.For) *> (For () <$> (expect TK.OpenParens *> parse) <*> (optional parse <* expect TK.Semicolon) <*> (optional parse <* expect TK.CloseParens) <*> parse)
      <|> expect (TK.Keyword TK.Switch) *> (Switch () <$> (expect TK.OpenParens *> parse <* expect TK.CloseParens) <*> parse)
      <|> Compound () <$> parse
      <|> Expression () <$> parse <* expect TK.Semicolon

-- <for-init> ::= <var-declaration> | [<exp>] ";"
instance Parser (ForInit ParserPhase) where
  parse :: ParserMonad (ForInit ParserPhase)
  parse = InitDecl <$> parse
      <|> InitExp  <$> optional parse <* expect TK.Semicolon

-- <label> ::= <identifier> ":"
--           | "case" <const> ":"
--           | "default" ":"
instance Parser (Label ParserPhase) where
  parse :: ParserMonad (Label ParserPhase)
  parse = expect (TK.Keyword TK.Case) *> (Case () <$> parse) <* expect TK.Colon
      <|> expect (TK.Keyword TK.Default) $> Default () <* expect TK.Colon
      <|> (Label () <$> parse) <* expect TK.Colon

-- <exp> ::= <assignment-exp>
instance Parser (Exp ParserPhase) where
  parse :: ParserMonad (Exp ParserPhase)
  parse = parseAssignmentExp

-- <assignment-exp> ::= <conditional-exp> | <unary-exp> <assignment-op> <assignment-exp>
parseAssignmentExp :: ParserMonad (Exp ParserPhase)
parseAssignmentExp = Assignment () <$> parseConditionalExp <*> parse <*> parseAssignmentExp
                 <|> parseConditionalExp

-- <conditional-exp> ::= <binary-exp> | <binary-exp> "?" <exp> ":" <conditional-exp>
parseConditionalExp :: ParserMonad (Exp ParserPhase)
parseConditionalExp = Conditional () <$> parseBinaryExp <*> (expect TK.QuestionMark *> parse) <*> (expect TK.Colon *> parseConditionalExp)
                  <|> parseBinaryExp

-- Parses binary operator expressions. It uses the operator precedence and associativity to properly parse the expression
-- <binary-exp> ::= <unary-exp> | <unary-exp> <binop> <unary-exp>
parseBinaryExp :: ParserMonad (Exp ParserPhase)
parseBinaryExp = precedenceClimb 0
  where precedenceClimb :: Natural -> ParserMonad (Exp ParserPhase)
        precedenceClimb basePrec =
          parseUnaryExp >>= \left -> catchST (const $ return left) (precedenceClimb' basePrec left)
          where precedenceClimb' :: Natural -> Exp ParserPhase -> ParserMonad (Exp ParserPhase)
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
                          precedenceClimb' minPrec (Binary () op left right)

-- <unary-exp> ::= <postfix-exp> | <unary-op> <unary-exp>
-- <unary-op> ::= "-" | "~" | "!" | "++" | "--"
parseUnaryExp :: ParserMonad (Exp ParserPhase)
parseUnaryExp = expect TK.Decrement  *> (Unary () (UnaryAssignmentOperator PreDecrement) <$> parseUnaryExp)
            <|> expect TK.Increment  *> (Unary () (UnaryAssignmentOperator PreIncrement) <$> parseUnaryExp)
            <|> expect TK.Minus      *> (Unary () Negate                                 <$> parseUnaryExp)
            <|> expect TK.Complement *> (Unary () Complement                             <$> parseUnaryExp)
            <|> expect TK.Not        *> (Unary () Not                                    <$> parseUnaryExp)
            <|> parsePostfixExp

-- <postfix-exp> ::= <primary-exp> | <postfix-exp> "++" | <postfix-exp> "--"
parsePostfixExp :: ParserMonad (Exp ParserPhase)
parsePostfixExp = parsePrimaryExp >>= consumePostfix
  where consumePostfix :: Exp ParserPhase -> ParserMonad (Exp ParserPhase)
        consumePostfix expr = expect TK.Decrement *> consumePostfix (Unary () (UnaryAssignmentOperator PostDecrement) expr)
                          <|> expect TK.Increment *> consumePostfix (Unary () (UnaryAssignmentOperator PostIncrement) expr)
                          <|> pure expr

-- <primary-exp> ::= <identifier> | <int-constant> | "(" <exp> ")" | <identifier> "(" [<argument-list> ] ")"
-- <argument-list> ::= <exp> {"," <exp>}
parsePrimaryExp :: ParserMonad (Exp ParserPhase)
parsePrimaryExp = FunctionCall () <$> parse <*> (expect TK.OpenParens *> argumentList <* expect TK.CloseParens)
              <|> Var          () <$> parse
              <|> Constant     () <$> parse
              <|> expect TK.OpenParens *> parse <* expect TK.CloseParens
  where argumentList :: ParserMonad [Exp ParserPhase]
        argumentList = (:) <$> parse <*> many (expect TK.Comma *> parse)
                   <|> pure []

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

