{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer.Token where

import           Data.Text (Text)
import           Location
import           Pretty    (PrettyPrinter (..))

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

data TokenType
  = Keyword Keyword
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
  | QuestionMark
  | Colon
  | Comma
  deriving (Show, Eq)

data Keyword
  = Int
  | Void
  | Return
  | If
  | Else
  | Goto
  | Do
  | While
  | For
  | Break
  | Continue
  | Switch
  | Case
  | Default
  deriving (Show, Eq)

tokenType :: Token -> TokenType
tokenType (Token t _ _) = t

scanKeyword :: Identifier -> Maybe Keyword
scanKeyword "int"      = Just Int
scanKeyword "void"     = Just Void
scanKeyword "return"   = Just Return
scanKeyword "if"       = Just If
scanKeyword "else"     = Just Else
scanKeyword "goto"     = Just Goto
scanKeyword "do"       = Just Do
scanKeyword "while"    = Just While
scanKeyword "for"      = Just For
scanKeyword "break"    = Just Break
scanKeyword "continue" = Just Continue
scanKeyword "switch"   = Just Switch
scanKeyword "case"     = Just Case
scanKeyword "default"  = Just Default
scanKeyword _          = Nothing

instance PrettyPrinter TokenType where
  pretty :: TokenType -> Text
  pretty (Keyword i)         = pretty i
  pretty (Identifier i)      = pretty i
  pretty (Constant i)        = pretty i
  pretty (String s)          = "\"" <> s <> "\""
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
  pretty QuestionMark        = "?"
  pretty Colon               = ":"
  pretty Comma               = ","

instance PrettyPrinter Keyword where
  pretty :: Keyword -> Text
  pretty Int      = "int"
  pretty Void     = "void"
  pretty Return   = "return"
  pretty If       = "if"
  pretty Else     = "else"
  pretty Goto     = "goto"
  pretty Do       = "do"
  pretty While    = "while"
  pretty For      = "for"
  pretty Break    = "break"
  pretty Continue = "continue"
  pretty Switch   = "switch"
  pretty Case     = "case"
  pretty Default  = "default"
