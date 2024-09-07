{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer.Error where

import           Lexer.Token
import           Pretty      (PrettyPrinter (..))

import           Data.Text   (Text, pack)
import           Error       (Error)

data LexerError = LexerError LexerErrorType Lexeme Location
  deriving (Show)

data LexerErrorType = ReachedEOF
                    | UnknownToken
                    | MalformedToken
                    | MalformedConstant String
                    | UnexpectedSymbol { expected :: Char
                                       , got      :: ScannedSymbol
                                       }
  deriving (Show)

instance PrettyPrinter LexerError where
  pretty :: LexerError -> Text
  pretty (LexerError ReachedEOF            lexeme location) = pretty location <> ": error: Reached EOF while scanning '" <> lexeme <> "' at "
  pretty (LexerError UnknownToken          lexeme location) = pretty location <> ": error: Unknown token '"              <> lexeme <> "' at "
  pretty (LexerError MalformedToken        lexeme location) = pretty location <> ": error: Malformed token '"            <> lexeme <> "' at "
  pretty (LexerError (MalformedConstant e) lexeme location) = pretty location <> ": error: Malformed constant literal '" <> lexeme <> "' at " <> ": " <> pack e
  pretty (LexerError UnexpectedSymbol {expected=e, got=g} lexeme location) = pretty location <> ": error: Expected symbol '" <> pretty e <> "' but got a '" <> pretty g <> "' in '" <> lexeme <> "' at "

instance Error LexerError

