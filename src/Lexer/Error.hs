{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer.Error where

import           Lexer.Token
import           Pretty      (PrettyPrinter (..))

import           Data.Text   (Text, pack)
import           Error       (Error)
import           Location

data LexerError = LexerError LexerErrorType Lexeme Location
  deriving (Show)

data LexerErrorType = ReachedEOF
                    | UnknownToken
                    | MalformedToken
                    | MalformedConstant String
                    | UnexpectedSymbol Char (Maybe Char)
  deriving (Show)

instance Locatable LexerError where
  locate :: LexerError -> Maybe Location
  locate (LexerError _ _ loc) = Just loc

instance PrettyPrinter LexerError where
  pretty :: LexerError -> Text
  pretty (LexerError ReachedEOF             lexeme _) = "Reached EOF while scanning '" <> lexeme <> "'"
  pretty (LexerError UnknownToken           lexeme _) = "Unknown token '"              <> lexeme <> "'"
  pretty (LexerError MalformedToken         lexeme _) = "Malformed token '"            <> lexeme <> "'"
  pretty (LexerError (MalformedConstant e)  lexeme _) = "Malformed constant literal '" <> lexeme <> "': " <> pack e
  pretty (LexerError (UnexpectedSymbol e g) lexeme _) = "Expected symbol " <> pretty e <> " but got " <> g' <> " in '" <> lexeme <> "'"
    where g' = case g of (Just c) -> pretty c
                         Nothing  -> "EOF"

instance Error LexerError

