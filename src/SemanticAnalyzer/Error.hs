{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module SemanticAnalyzer.Error where

import           Parser.AST (Identifier)

data SemanticError = DuplicateDeclaration Identifier
                   | UndefinedIdentifierUse Identifier
                   | InvalidLHS
                   | NotConstantExpression
                   | DuplicateLabelDeclaration Identifier
                   | UndefinedLabelUse Identifier
                   | StatementOutsideLoop
                   | StatementOutsideSwitch
                   | StatementOutsideSwitchOrLoop
                   | DuplicateCaseValue
                   | DuplicateDefaultValue
                   | NestedFunctionDefinition
                   | IncompatibleTypes
                   | FunctionDefinedMoreThanOnce
                   | FunctionUsedAsVariable
                   | VariableUsedAsFunction
  deriving (Show)

