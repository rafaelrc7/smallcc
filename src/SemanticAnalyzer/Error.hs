module SemanticAnalyzer.Error where

import           Data.Text  (Text)
import           Parser.AST (Exp)

data SemanticError = DuplicateVariableDeclaration Text
                   | UndefinedVariableUse Text
                   | InvalidLHS Exp
  deriving (Show)

