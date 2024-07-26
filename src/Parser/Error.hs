module Parser.Error (ParserError(..)) where

import           Lexer.Token

data ParserError = UnexpectedToken { expected :: Token
                                   , got      :: Token
                                   }
                 | UnexpectedEOF { expected :: Token }
                 | ExpectedEOF { got :: Token }
  deriving (Show)
