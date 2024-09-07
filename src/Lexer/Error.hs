{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer.Error where

import           Lexer.Token
import           Pretty      (PrettyPrinter (..))

import           Data.Text   (Text, pack)

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
  pretty (LexerError ReachedEOF            lexeme location) = "Reached EOF while scanning '" <> lexeme <> "' at " <> pretty location
  pretty (LexerError UnknownToken          lexeme location) = "Unknown token '"              <> lexeme <> "' at " <> pretty location
  pretty (LexerError MalformedToken        lexeme location) = "Malformed token '"            <> lexeme <> "' at " <> pretty location
  pretty (LexerError (MalformedConstant e) lexeme location) = "Malformed constant literal '" <> lexeme <> "' at " <> pretty location <> ": " <> pack e
  pretty (LexerError UnexpectedSymbol {expected=e, got=g} lexeme location) = "Expected symbol '" <> pretty e <> "' but got a '" <> pretty g <> "' in '" <> lexeme <> "' at " <> pretty location

