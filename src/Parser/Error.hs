module Parser.Error (ParserError(..)) where

import           Data.Text   (Text)
import           Lexer.Token

data ParserError = UnexpectedToken { expected :: Text
                                   , got      :: Token
                                   }
                 | UnexpectedEOF { expected :: Text }
                 | ExpectedEOF { got :: Token }
  deriving (Show)
