module SemanticAnalyzer.Error where

import           Parser.AST (Exp, Identifier)

data SemanticError = DuplicateVariableDeclaration Identifier
                   | UndefinedVariableUse Identifier
                   | InvalidLHS Exp
                   | DuplicateLabelDeclaration Identifier
                   | UndefinedLabelUse Identifier
                   | BreakOutsideLoop
                   | ContinueOutsideLoop
  deriving (Show)

