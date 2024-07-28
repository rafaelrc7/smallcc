module Parser.Error (ParserError(..)) where

import           Lexer.Token
import Data.Text (Text)

data ParserError = UnexpectedToken { expected :: Text
                                   , got      :: Token
                                   }
                 | UnexpectedEOF { expected :: Text }
                 | ExpectedEOF { got :: Token }
  deriving (Show)
