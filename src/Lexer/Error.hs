{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer.Error where

import Lexer.Token
import Pretty (PrettyPrinter(..))

import Data.Text (Text, pack)

data LexerError = LexerError LexerErrorType Lexeme Location
  deriving (Show)

data LexerErrorType = EOF
                    | UnknownToken
                    | MalformedToken
                    | MalformedConstant String
  deriving (Show)

instance PrettyPrinter LexerError where
  pretty :: LexerError -> Text
  pretty (LexerError EOF                   lexeme location) = "Reached EOF while scanning '" <> lexeme <> "' at " <> pretty location
  pretty (LexerError UnknownToken          lexeme location) = "Unknown token '"              <> lexeme <> "' at " <> pretty location
  pretty (LexerError MalformedToken        lexeme location) = "Malformed token '"            <> lexeme <> "' at " <> pretty location
  pretty (LexerError (MalformedConstant e) lexeme location) = "Malformed constant literal '" <> lexeme <> "' at " <> pretty location <> ": " <> pack e

