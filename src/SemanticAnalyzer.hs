module SemanticAnalyzer where

import           Control.Monad.Except   (runExcept)
import           Control.Monad.State    (evalStateT)
import           Parser.AST             (Program)
import           SemanticAnalyzer.AST   (SemanticAnalyzer (checkLabels, resolve),
                                         emptyEnvironment)
import           SemanticAnalyzer.Error (SemanticError)

semanticAnalyze :: Program -> Either SemanticError Program
semanticAnalyze prog = runExcept $ evalStateT (resolve prog >>= checkLabels) emptyEnvironment

