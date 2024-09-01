{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module SemanticAnalyzer.AST where

import           Parser.AST             (BlockItem (..), Declaration (..),
                                         Exp (..),
                                         FunctionDefinition (Function, funcBody),
                                         Identifier, Program (..),
                                         Statement (..))
import           SemanticAnalyzer.Error (SemanticError (..))

import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Text              (Text)
import qualified Data.Text              as T
import           GHC.Num.Natural        (Natural)

type VarMap = Map Identifier Identifier

data State = State { stateLastVar :: Natural
                   , stateVarMap  :: VarMap
                   }

emptyState :: State
emptyState = State { stateLastVar = 0
                   , stateVarMap  = M.empty
                   }

makeUnique :: State -> Text -> (Text, State)
makeUnique st@State { stateLastVar = lastVar } var = (uniqueVar, st { stateLastVar = newUniqueVar })
  where newUniqueVar = lastVar + 1
        uniqueVar = "var." <> var <> "." <> T.pack (show newUniqueVar)

newVar :: State -> Text -> (Text, State)
newVar st var = (uniqueVar, st' { stateVarMap = varMap' })
  where (uniqueVar, st'@State { stateVarMap=varMap }) = makeUnique st var
        varMap' = M.insert var uniqueVar varMap

class SemanticAnalyzer a where
  resolve :: State -> a -> Either SemanticError (a, State)

instance SemanticAnalyzer Program where
  resolve :: State -> Program -> Either SemanticError (Program, State)
  resolve st (Program func) = resolve st func >>= \(func', st') -> Right (Program func', st')

instance SemanticAnalyzer FunctionDefinition where
  resolve :: State -> FunctionDefinition -> Either SemanticError (FunctionDefinition, State)
  resolve st func@Function { funcBody = body } = result >>= \(body', st') -> Right (func { funcBody = body' }, st')
    where result = foldl (\acc item -> acc >>= \(body', st') ->
                           resolve st' item >>= \(item', st'') ->
                           Right (item':body', st''))
                         (Right ([], st))
                         body

instance SemanticAnalyzer BlockItem where
  resolve :: State -> BlockItem -> Either SemanticError (BlockItem, State)
  resolve st (Stmt stmt) = resolve st stmt >>= \(stmt', st') -> Right (Stmt stmt', st')
  resolve st (Dec decl)  = resolve st decl >>= \(decl', st') -> Right (Dec  decl', st')

instance SemanticAnalyzer Statement where
  resolve :: State -> Statement -> Either SemanticError (Statement, State)
  resolve st (Return expr) = resolve st expr >>= \(expr', st') -> Right (Return expr', st')
  resolve st (Expression expr) = resolve st expr >>= \(expr', st') -> Right (Expression expr', st')
  resolve st Null = Right (Null, st)

instance SemanticAnalyzer Declaration where
  resolve :: State -> Declaration -> Either SemanticError (Declaration, State)
  resolve st@State { stateVarMap  = varMap } (Declaration var initialization)
    | var `M.member` varMap = Left $ DuplicateVariableDeclaration var
    | otherwise = resolveInitialization >>= \(initialization', st'') -> Right (Declaration uniqueVar initialization', st'')
      where (uniqueVar, st') = newVar st var
            resolveInitialization =
              case initialization of
                Nothing   -> Right (Nothing, st')
                Just expr -> resolve st' expr >>= \(expr', st'') ->
                             Right (Just expr', st'')

instance SemanticAnalyzer Exp where
  resolve :: State -> Exp -> Either SemanticError (Exp, State)
  resolve st expr@(Constant _) = Right (expr, st)
  resolve st (Unary op expr) = resolve st expr >>= \(expr', st') -> Right (Unary op expr', st')
  resolve st (Binary op exprl exprr) =
    do (exprl', st')  <- resolve st exprl
       (exprr', st'') <- resolve st' exprr
       Right (Binary op exprl' exprr', st'')
  resolve st@State { stateVarMap = varMap } (Var var) =
    case M.lookup var varMap of
      Nothing   -> Left $ UndefinedVariableUse var
      Just var' -> Right (Var var', st)
  resolve st (Assignment var@(Var _) expr) =
    do (var',  st')  <- resolve st var
       (expr', st'') <- resolve st' expr
       Right (Assignment var' expr', st'')
  resolve _ (Assignment lval _) = Left $ InvaliedLHS lval
