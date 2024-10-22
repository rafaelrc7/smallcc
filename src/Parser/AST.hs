{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parser.AST where

import           Data.Text            (Text)
import qualified Data.Text            as T

import           Control.Monad.Except (ExceptT, MonadError (..), handleError)
import           Control.Monad.State  (MonadState (get, put), State)
import           Data.Functor         ((<&>))
import           Numeric.Natural      (Natural)

import           Lexer.Token          (Token (..), TokenType, tokenType)
import qualified Lexer.Token          as TK
import           Parser.Error         (ParserError (..))
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

type ParserState = [Token]
type ParserMonad a = ExceptT ParserError (State ParserState) a

class Parser a where
  parse :: ParserMonad a

(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

catchST :: (ParserError -> ParserMonad a) -> ParserMonad a -> ParserMonad a
catchST handler action = get >>= \st -> handleError (\e -> put st >> handler e) action

expectEOF :: ParserMonad ()
expectEOF = get >>= \case
  [] -> return ()
  (t:_) -> throwError $ UnexpectedToken "EOF" t

expect :: TokenType -> ParserMonad ()
expect s = get >>= \case
  [] -> throwError $ UnexpectedEOF s'
  (t@(Token tt _ _):ts)
         | tt == s -> put ts
         | otherwise -> throwError $ UnexpectedToken s' t
  where s' = T.pack $ show s

expect' :: [TokenType] -> ParserMonad ()
expect' = foldr ((>>) . expect) (return ())

peek :: ParserMonad Token
peek = get >>= \case
  []    -> throwError $ UnexpectedEOF "<token>"
  (t:_) -> return t

pop :: ParserMonad Token
pop = get >>= \case
  []     -> throwError $ UnexpectedEOF "<token>"
  (t:ts) -> put ts >> return t

-- <program> ::= <function>
instance Parser Program where
  parse :: ParserMonad Program
  parse = parse >>= \p -> expectEOF >> return (Program p)

-- <function> ::= "int" <identifier> "(" "void" ")" "{" {<block-item>} "}"
-- <block-item> ::= <statement> | <declaration>
instance Parser FunctionDefinition where
  parse :: ParserMonad FunctionDefinition
  parse = do expect (TK.Keyword TK.Int)
             name <- parse
             expect' [ TK.OpenParens, TK.Keyword TK.Void, TK.CloseParens, TK.OpenBrace ]
             bis <- parseBlockItensM
             expect TK.CloseBrace
             return Function { funcName = name
                             , funcBody = bis
                             }

-- {<block_item>} ::= <block_item>*
parseBlockItensM :: ParserMonad [BlockItem]
parseBlockItensM = peek >>= tokenType |> \case
                       TK.CloseBrace -> return []
                       _             -> (:) <$> parse <*> parseBlockItensM

-- <block_item> ::= <statement> | <declaration>
instance Parser BlockItem where
  parse :: ParserMonad BlockItem
  parse = peek >>= \case
             Token (TK.Keyword TK.Int) _ _ -> parse <&> Dec
             _                             -> parse <&> Stmt

-- <statement> ::= "return" <exp> ";"
instance Parser Statement where
  parse :: ParserMonad Statement
  parse = peek >>= \t -> case tokenType t of
               TK.Semicolon           -> pop >> return Null
               (TK.Keyword TK.Return) -> pop >> parse >>= \expr -> expect TK.Semicolon >> return (Return expr)
               _                      -> parse >>= \expr -> expect TK.Semicolon >> return (Expression expr)

-- <declaration> ::= "int" <identifier> ["=" <exp>] ";"
instance Parser Declaration where
  parse :: ParserMonad Declaration
  parse = do expect (TK.Keyword TK.Int)
             name <- parse
             peek >>= tokenType |> \case
                 TK.Semicolon -> pop >> return (Declaration name Nothing)
                 _            -> do expect TK.Equals
                                    expr <- parse
                                    expect TK.Semicolon
                                    return (Declaration name (Just expr))

-- <exp> ::= <factor> | <exp> <binop> <exp>
instance Parser Exp where
  parse :: ParserMonad Exp
  parse = precedenceClimbM 0

precedenceClimbM :: Natural -> ParserMonad Exp
precedenceClimbM basePrec =
  parseFactorM >>= \left -> catchST (const $ return left) (precedenceClimbM' basePrec left)
  where precedenceClimbM' :: Natural -> Exp -> ParserMonad Exp
        precedenceClimbM' minPrec left =
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
                  right <- precedenceClimbM nextPrecedence
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
                  precedenceClimbM' minPrec expr

-- <factor> ::= <int> | <unop> <exp> | <exp> <unop> | "(" <exp> ")"
-- <unop> ::= "-" | "~" | "!" | "++" | "--"
parseFactorM :: ParserMonad Exp
parseFactorM = parseFactorM' >>= consumePostfix
  where parseFactorM' :: ParserMonad Exp
        parseFactorM' =
          peek >>= \t -> case tokenType t of
               TK.Decrement      -> pop >> parseFactorM <&> PreAssignment Decrement
               TK.Increment      -> pop >> parseFactorM <&> PreAssignment Increment
               TK.Minus          -> pop >> parseFactorM <&> Unary Negate
               TK.Complement     -> pop >> parseFactorM <&> Unary Complement
               TK.Not            -> pop >> parseFactorM <&> Unary Not
               TK.OpenParens     -> pop >> parse >>= \expr -> expect TK.CloseParens >> return expr
               (TK.Constant _)   -> parse <&> Constant
               (TK.Identifier _) -> parse <&> Var
               _                 -> pop >> throwError (UnexpectedToken "<factor>" t)

        consumePostfix :: Exp -> ParserMonad Exp
        consumePostfix expr = peek >>= tokenType |> \case
                                  TK.Decrement -> pop >> consumePostfix (PostAssignment Decrement expr)
                                  TK.Increment -> pop >> consumePostfix (PostAssignment Increment expr)
                                  _            -> return expr

-- <binop> ::= "-" | "+" | "*" | "/" | "%" | "&&" | "||"
--           | "==" | "!=" | "<" | "<=" | ">" | ">=" | "="
--           | ">>" | "<<" | "&" | "|" | "^"
instance Parser BinaryOperator where
  parse :: ParserMonad BinaryOperator
  parse = peek >>= \t -> case tokenType t of
      TK.Or             -> pop >> return Or
      TK.And            -> pop >> return And
      TK.BitOr          -> pop >> return BitOr
      TK.BitXOR         -> pop >> return BitXOR
      TK.BitAnd         -> pop >> return BitAnd
      TK.EqualsTo       -> pop >> return EqualsTo
      TK.NotEqualsTo    -> pop >> return NotEqualsTo
      TK.Less           -> pop >> return Less
      TK.LessOrEqual    -> pop >> return LessOrEqual
      TK.Greater        -> pop >> return Greater
      TK.GreaterOrEqual -> pop >> return GreaterOrEqual
      TK.BitShiftLeft   -> pop >> return BitShiftLeft
      TK.BitShiftRight  -> pop >> return BitShiftRight
      TK.Plus           -> pop >> return Add
      TK.Minus          -> pop >> return Subtract
      TK.Asterisk       -> pop >> return Multiply
      TK.ForwardSlash   -> pop >> return Divide
      TK.Percent        -> pop >> return Remainder
      _                 -> parse <&> BinaryAssignmentOperator

instance Parser BinaryAssignmentOperator where
  parse :: ParserMonad BinaryAssignmentOperator
  parse = pop >>= \t -> case tokenType t of
            TK.Equals              -> return Assign
            TK.IncAssign           -> return IncAssign
            TK.DecAssign           -> return DecAssign
            TK.MulAssign           -> return MulAssign
            TK.DivAssign           -> return DivAssign
            TK.ModAssign           -> return ModAssign
            TK.BitAndAssign        -> return BitAndAssign
            TK.BitOrAssign         -> return BitOrAssign
            TK.BitXORAssign        -> return BitXORAssign
            TK.BitShiftLeftAssign  -> return BitShiftLeftAssign
            TK.BitShiftRightAssign -> return BitShiftRightAssign
            _                      -> throwError $ UnexpectedToken "<binop>" t

-- <int> ::= Tokens.Constant
instance Parser Constant where
  parse :: ParserMonad Constant
  parse = get >>= \case
    (Token (TK.Constant c) _ _ : ts) -> put ts >> return (CInt c)
    (t:_)                              -> throwError $ UnexpectedToken "<int>" t
    []                                 -> throwError $ UnexpectedEOF "<int>"

-- <identifier> ::= Tokens.Identifier
instance Parser Identifier where
  parse :: ParserMonad Identifier
  parse = get >>= \case
    (Token (TK.Identifier n) _ _ : ts) -> put ts >> return n
    (t : _)                            -> throwError $ UnexpectedToken "<identifier>" t
    []                                 -> throwError $ UnexpectedEOF "<identifier>"

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
  pretty Null = ";\n"

instance PrettyPrinter Declaration where
  pretty :: Declaration -> Text
  pretty (Declaration name Nothing) = "int " <> name <> ";\n";
  pretty (Declaration name (Just i)) = "int " <> name <> " = " <> pretty i <> ";\n";

instance PrettyPrinter Exp where
  pretty :: Exp -> Text
  pretty (Constant val) = pretty val
  pretty (Unary op expr) = pretty op <> pretty expr
  pretty (Binary op exprl exprr) = "(" <> T.intercalate " " [pretty exprl, pretty op, pretty exprr] <> ")"
  pretty (Var var) = var
  pretty (Assignment lhs rhs) = "(" <> lhs' <> " = " <> rhs' <> ")"
    where lhs' = pretty lhs
          rhs' = pretty rhs
  pretty (PreAssignment op var) = "(" <> op' <> var' <> ")"
    where var' = pretty var
          op'  = pretty op
  pretty (PostAssignment op var) = "(" <> var' <> op' <> ")"
    where var' = pretty var
          op'  = pretty op

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
  pretty Decrement = "--"
  pretty Increment = "++"

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
  pretty (BinaryAssignmentOperator IncAssign)           = "+="
  pretty (BinaryAssignmentOperator DecAssign)           = "-="
  pretty (BinaryAssignmentOperator MulAssign)           = "*="
  pretty (BinaryAssignmentOperator DivAssign)           = "/="
  pretty (BinaryAssignmentOperator ModAssign)           = "%="
  pretty (BinaryAssignmentOperator BitAndAssign)        = "&="
  pretty (BinaryAssignmentOperator BitOrAssign)         = "|="
  pretty (BinaryAssignmentOperator BitXORAssign)        = "^="
  pretty (BinaryAssignmentOperator BitShiftLeftAssign)  = "<<="
  pretty (BinaryAssignmentOperator BitShiftRightAssign) = ">>="

