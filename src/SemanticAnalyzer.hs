module SemanticAnalyzer where

import           Control.Monad.Except                   (runExcept)
import           Control.Monad.State                    (evalStateT)
import           Parser.AST                             (Program)
import           Parser.ParserMonad                     (ParserPhase)
import           SemanticAnalyzer.Error                 (SemanticError)
import           SemanticAnalyzer.SemanticAnalyzerMonad (LabelResolver (resolveLabelDeclaration, resolveLabelReference),
                                                         SwitchResolver (resolveSwitch),
                                                         SwitchResolvingPhase,
                                                         VariableResolver (resolveVariable),
                                                         emptyEnvironment)

semanticAnalyze :: Program ParserPhase -> Either SemanticError (Program SwitchResolvingPhase)
semanticAnalyze prog = runExcept $ evalStateT (resolveVariable prog >>= resolveLabelDeclaration >>= resolveLabelReference >>= resolveSwitch) emptyEnvironment

