module SemanticAnalyzer.Error where

import           Data.Text  (Text)
import           Parser.AST (Exp)

data SemanticError = DuplicateIdentifierDeclaration Text
                   | UndefinedIdentifierUse Text
                   | InvalidLHS Exp
  deriving (Show)

