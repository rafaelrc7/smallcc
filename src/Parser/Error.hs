{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Error (ParserError(..)) where

import           Data.Text   (Text)
import           Error       (Error)
import           Lexer.Token (Token (..))
import           Location    (Locatable (..), Location)
import           Pretty      (PrettyPrinter (..))

data ParserError = UnexpectedToken Text Token
                 | UnexpectedEOF   Text Location
  deriving (Show)

instance Locatable ParserError where
  locate :: ParserError -> Location
  locate (UnexpectedToken _ (Token _ _ loc)) = loc
  locate (UnexpectedEOF   _ loc)             = loc

instance PrettyPrinter ParserError where
  pretty :: ParserError -> Text
  pretty (UnexpectedToken e (Token g _ _)) = "Expected token '" <> e <> "' but got a '" <> pretty g <> "'"
  pretty (UnexpectedEOF   e _)             = "Expected token '" <> e <> "' but reached EOF"

instance Error ParserError

