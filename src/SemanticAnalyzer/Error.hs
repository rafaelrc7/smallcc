{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module SemanticAnalyzer.Error where

import           Parser.AST (Exp, Forall, Identifier)

data SemanticError p = DuplicateVariableDeclaration Identifier
                     | UndefinedVariableUse Identifier
                     | InvalidLHS (Exp p)
                     | DuplicateLabelDeclaration Identifier
                     | UndefinedLabelUse Identifier
                     | StatementOutsideLoop
                     | StatementOutsideSwitch
                     | StatementOutsideSwitchOrLoop
deriving instance (Forall Show p) => Show (SemanticError p)

