{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Error (ParserError(..)) where

import           Data.Text   (Text)
import           Lexer.Token (Token (..))
import           Pretty      (PrettyPrinter (..))

data ParserError = UnexpectedToken { expected :: Text
                                   , got      :: Token
                                   }
                 | UnexpectedEOF { expected :: Text }
                 | ExpectedEOF   { got :: Token }
  deriving (Show)

instance PrettyPrinter ParserError where
  pretty :: ParserError -> Text
  pretty UnexpectedToken {expected=e, got=Token g _ loc} = "Expected token '" <> e <> "' but got a '" <> pretty g <> "' at " <> pretty loc
  pretty UnexpectedEOF   {expected=e}                    = "Expected token '" <> e <> "' but reached EOF"
  pretty ExpectedEOF     {got=Token g _ loc}             = "Expected EOF but got '" <> pretty g <> "at " <> pretty loc

