{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module SemanticAnalyzer.Error where

import           Parser.AST (Identifier)

data SemanticError = DuplicateVariableDeclaration Identifier
                   | UndefinedVariableUse Identifier
                   | InvalidLHS
                   | NotConstantExpression
                   | DuplicateLabelDeclaration Identifier
                   | UndefinedLabelUse Identifier
                   | StatementOutsideLoop
                   | StatementOutsideSwitch
                   | StatementOutsideSwitchOrLoop
                   | DuplicateCaseValue
                   | DuplicateDefaultValue
  deriving (Show)

