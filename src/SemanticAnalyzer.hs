module SemanticAnalyzer where

import           Control.Monad.Except                   (runExcept)
import           Control.Monad.State                    (evalStateT)
import qualified Parser.AST                             as P
import           SemanticAnalyzer.AST                   (Program)
import           SemanticAnalyzer.Error                 (SemanticError)
import           SemanticAnalyzer.SemanticAnalyzerMonad (LabelResolver (resolveLabelDeclaration, resolveLabelReference),
                                                         StatementLabeler (labelStatement),
                                                         VariableResolver (resolveVariable),
                                                         emptyEnvironment)

semanticAnalyze :: P.Program -> Either SemanticError Program
semanticAnalyze prog = runExcept $ evalStateT (resolveVariable prog >>= resolveLabelDeclaration >>= resolveLabelReference >>= labelStatement) emptyEnvironment

