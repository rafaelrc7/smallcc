module SemanticAnalyzer where

import           Control.Monad.Except                   (runExcept)
import           Control.Monad.State                    (evalStateT)
import           Parser.AST                             (Program)
import           Parser.ParserMonad                     (ParserPhase)
import           SemanticAnalyzer.Error                 (SemanticError)
import           SemanticAnalyzer.SemanticAnalyzerMonad (IdentifierResolver (..),
                                                         LabelResolver (..),
                                                         SwitchResolver (..),
                                                         TypeChecker (resolveTypes),
                                                         TypeCheckingPhase,
                                                         emptyEnvironment)

semanticAnalyze :: Program ParserPhase -> Either SemanticError (Program TypeCheckingPhase)
semanticAnalyze prog = runExcept $ evalStateT (resolveIdentifiers prog >>= resolveLabelDeclaration >>= resolveLabelReference >>= resolveSwitch >>= resolveTypes) emptyEnvironment
