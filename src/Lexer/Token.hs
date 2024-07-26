{-# LANGUAGE OverloadedStrings #-}

module Lexer.Token where

import qualified Data.Text as T

data Token = Keyword Keyword
           | Identifier T.Text
           | Constant Int
           | OpenParens
           | CloseParens
           | OpenBrace
           | CloseBrace
           | Semicolon
  deriving (Show, Eq)

data Keyword = Int
             | Void
             | Return
  deriving (Show, Eq)

parseKeyword :: T.Text -> Maybe Keyword
parseKeyword "int"    = Just Int
parseKeyword "void"   = Just Void
parseKeyword "return" = Just Return
parseKeyword _        = Nothing

