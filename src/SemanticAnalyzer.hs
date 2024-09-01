module SemanticAnalyzer where

import           Parser.AST             (Program)
import           SemanticAnalyzer.AST   (SemanticAnalyzer (resolve), emptyState)
import           SemanticAnalyzer.Error (SemanticError)

semanticAnalyze :: Program -> Either SemanticError Program
semanticAnalyze prog = resolve emptyState prog >>= Right . fst

