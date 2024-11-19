module SemanticAnalyzer where

import           Control.Monad.Except                   (runExcept)
import           Control.Monad.State                    (evalStateT)
import           Parser.AST                             (Program)
import           Parser.ParserMonad                     (ParserPhase)
import           SemanticAnalyzer.Error                 (SemanticError)
import           SemanticAnalyzer.SemanticAnalyzerMonad (LabelResolver (resolveLabelDeclaration, resolveLabelReference),
                                                         LabelResolvingPhase,
                                                         VariableResolver (resolveVariable),
                                                         VariableResolvingPhase,
                                                         emptyEnvironment)

semanticAnalyze :: Program ParserPhase -> Either (SemanticError VariableResolvingPhase) (Program LabelResolvingPhase)
semanticAnalyze prog = runExcept $ evalStateT (resolveVariable prog >>= resolveLabelDeclaration >>= resolveLabelReference) emptyEnvironment

