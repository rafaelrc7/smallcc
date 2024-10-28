{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer.Token where

import           Pretty    (PrettyPrinter (..))

import           Data.Text (Text)
import           Location

type Lexeme = Text
type Identifier = Text

data Token = Token TokenType Lexeme Location
  deriving (Show, Eq)

instance Ord Token where
  compare :: Token -> Token -> Ordering
  compare (Token _ _ loc1) (Token _ _ loc2) = compare loc1 loc2

instance Locatable Token where
  locate :: Token -> Maybe Location
  locate (Token _ _ loc) = Just loc

data TokenType = Keyword Keyword
               | Identifier Identifier
               | Constant Int
               | String Text
               | OpenParens
               | CloseParens
               | OpenBrace
               | CloseBrace
               | Semicolon
               | Complement
               | Decrement
               | Increment
               | Minus
               | Plus
               | Asterisk
               | ForwardSlash
               | Percent
               | And
               | BitAnd
               | Or
               | BitOr
               | BitXOR
               | Less
               | LessOrEqual
               | BitShiftLeft
               | Greater
               | GreaterOrEqual
               | BitShiftRight
               | Not
               | NotEqualsTo
               | EqualsTo
               | Equals
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
  deriving (Show, Eq)

data Keyword = Int
             | Void
             | Return
  deriving (Show, Eq)

tokenType :: Token -> TokenType
tokenType (Token t _ _) = t

scanKeyword :: Identifier -> Maybe Keyword
scanKeyword "int"    = Just Int
scanKeyword "void"   = Just Void
scanKeyword "return" = Just Return
scanKeyword _        = Nothing

instance PrettyPrinter TokenType where
  pretty :: TokenType -> Text
  pretty (Keyword    i)      = pretty i
  pretty (Identifier i)      = pretty i
  pretty (Constant   i)      = pretty i
  pretty (String     s)      = "\"" <> s <> "\""
  pretty OpenParens          = "("
  pretty CloseParens         = ")"
  pretty OpenBrace           = "{"
  pretty CloseBrace          = "}"
  pretty Semicolon           = ";"
  pretty Complement          = "~"
  pretty Decrement           = "--"
  pretty Increment           = "++"
  pretty Minus               = "-"
  pretty Plus                = "+"
  pretty Asterisk            = "*"
  pretty ForwardSlash        = "/"
  pretty Percent             = "%"
  pretty And                 = "&&"
  pretty BitAnd              = "&"
  pretty Or                  = "||"
  pretty BitOr               = "|"
  pretty BitXOR              = "^"
  pretty Less                = "<"
  pretty LessOrEqual         = "<="
  pretty BitShiftLeft        = "<<"
  pretty Greater             = ">"
  pretty GreaterOrEqual      = ">="
  pretty BitShiftRight       = ">>"
  pretty Not                 = "!"
  pretty NotEqualsTo         = "!="
  pretty EqualsTo            = "=="
  pretty Equals              = "="
  pretty IncAssign           = "+="
  pretty DecAssign           = "-="
  pretty MulAssign           = "*="
  pretty DivAssign           = "/="
  pretty ModAssign           = "%="
  pretty BitAndAssign        = "&="
  pretty BitOrAssign         = "|="
  pretty BitXORAssign        = "^="
  pretty BitShiftLeftAssign  = "<<="
  pretty BitShiftRightAssign = ">>="

instance PrettyPrinter Keyword where
  pretty :: Keyword -> Text
  pretty Int    = "int"
  pretty Void   = "void"
  pretty Return = "return"

