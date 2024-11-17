{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module SemanticAnalyzer.AST where

import           Parser.AST             (Block (Block), BlockItem (..),
                                         Declaration (..), Exp (..),
                                         FunctionDefinition (Function),
                                         Identifier, Program (..),
                                         Statement (..),
                                         UnaryOperator (UnaryAssignmentOperator))
import           SemanticAnalyzer.Error (SemanticError (..))

import           Control.Monad.Except   (Except, MonadError (..))
import           Control.Monad.State    (StateT, gets, modify)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import           Numeric.Natural        (Natural)

type IdentifierMap = Map Identifier Identifier

emptyIdentifierMap :: IdentifierMap
emptyIdentifierMap = M.empty

data Environment = Environment { envFunctionName       :: Maybe Identifier
                               , envVarCounter         :: Natural
                               , envCurrentScopeVarEnv :: IdentifierMap
                               , envUpperScopesVarEnvs :: [IdentifierMap]
                               , envLabelCounter       :: Natural
                               , envLabelEnv           :: IdentifierMap
                               }

emptyEnvironment :: Environment
emptyEnvironment = Environment { envFunctionName       = Nothing
                               , envVarCounter         = 0
                               , envCurrentScopeVarEnv = emptyIdentifierMap
                               , envUpperScopesVarEnvs = []
                               , envLabelCounter       = 0
                               , envLabelEnv           = emptyIdentifierMap
                               }

type SemanticAnalyzerMonad a = StateT Environment (Except SemanticError) a

newVar :: Identifier -> SemanticAnalyzerMonad Identifier
newVar var =
  do varCounter <- gets envVarCounter
     varEnv     <- gets envCurrentScopeVarEnv
     if var `M.member` varEnv then
       throwError $ DuplicateIdentifierDeclaration var
     else do
       let varCounter' = succ varCounter
       let var' = "var." <> var <> "." <> T.pack (show varCounter')
       let varEnv' = M.insert var var' varEnv
       modify (\env -> env { envCurrentScopeVarEnv = varEnv', envVarCounter = varCounter' })
       return var'

newLabel :: Identifier -> SemanticAnalyzerMonad Identifier
newLabel label =
  do functionName <- gets envFunctionName
     labelCounter <- gets envLabelCounter
     labelEnv     <- gets envLabelEnv
     if label `M.member` labelEnv then
       throwError $ DuplicateIdentifierDeclaration label
     else do
       let labelCounter' = succ labelCounter
       let label' = ".L" <> fromMaybe "" functionName <> "." <> label <> "." <> T.pack (show labelCounter')
       let labelEnv' = M.insert label label' labelEnv
       modify (\env -> env { envLabelEnv = labelEnv', envLabelCounter = labelCounter' })
       return label'

getVar :: Identifier -> SemanticAnalyzerMonad Identifier
getVar var = do m <- gets envCurrentScopeVarEnv
                case M.lookup var m of
                  Nothing   -> throwError $ UndefinedIdentifierUse var
                  Just var' -> return var'

getLabel :: Identifier -> SemanticAnalyzerMonad Identifier
getLabel label = do m <- gets envLabelEnv
                    case M.lookup label m of
                      Nothing     -> throwError $ UndefinedIdentifierUse label
                      Just label' -> return label'

class SemanticAnalyzer a where
  resolve :: a -> SemanticAnalyzerMonad a
  checkLabels :: a -> SemanticAnalyzerMonad a

instance SemanticAnalyzer Program where
  resolve :: Program -> SemanticAnalyzerMonad Program
  resolve (Program func) = Program <$> resolve func

  checkLabels :: Program -> SemanticAnalyzerMonad Program
  checkLabels (Program func) = Program <$> checkLabels func

instance SemanticAnalyzer FunctionDefinition where
  resolve :: FunctionDefinition -> SemanticAnalyzerMonad FunctionDefinition
  resolve (Function name body) =
    do modify (\env -> env { envFunctionName = Just name })
       Function name <$> resolve body

  checkLabels :: FunctionDefinition -> SemanticAnalyzerMonad FunctionDefinition
  checkLabels (Function name body) =
    do modify (\env -> env { envFunctionName = Just name })
       Function name <$> checkLabels body

instance SemanticAnalyzer Block where
  resolve :: Block -> SemanticAnalyzerMonad Block
  resolve (Block blockItens) = Block <$> mapM resolve blockItens

  checkLabels :: Block -> SemanticAnalyzerMonad Block
  checkLabels (Block blockItens) = Block <$> mapM checkLabels blockItens

instance SemanticAnalyzer BlockItem where
  resolve :: BlockItem -> SemanticAnalyzerMonad BlockItem
  resolve (Stmt stmt) = Stmt <$> resolve stmt
  resolve (Dec  dec)  = Dec  <$> resolve dec

  checkLabels :: BlockItem -> SemanticAnalyzerMonad BlockItem
  checkLabels (Stmt stmt) = Stmt <$> checkLabels stmt
  checkLabels (Dec dec)   = Dec <$> checkLabels dec

instance SemanticAnalyzer Statement where
  resolve :: Statement -> SemanticAnalyzerMonad Statement
  resolve (Return expr)     = Return <$> resolve expr
  resolve (Expression expr) = Expression <$> resolve expr
  resolve (If cond conseq altern) = If <$> resolve cond <*> resolve conseq <*>
    case altern of Just altern' -> Just <$> resolve altern'
                   Nothing      -> pure Nothing
  resolve (Compound block) = Compound <$> resolve block
  resolve Null = pure Null
  resolve (Label label) = Label <$> newLabel label
  resolve g@(Goto _) = pure g

  checkLabels :: Statement -> SemanticAnalyzerMonad Statement
  checkLabels (Goto label)      = Goto <$> getLabel label
  checkLabels (Return expr)     = Return <$> checkLabels expr
  checkLabels (Expression expr) = Expression <$> checkLabels expr
  checkLabels l@(Label _)       = pure l
  checkLabels (Compound block)  = Compound <$> checkLabels block
  checkLabels Null              = pure Null
  checkLabels (If cond conse altern) = If <$> checkLabels cond <*> checkLabels conse <*>
    case altern of Just altern' -> Just <$> checkLabels altern'
                   Nothing      -> pure Nothing

instance SemanticAnalyzer Declaration where
  resolve :: Declaration -> SemanticAnalyzerMonad Declaration
  resolve (Declaration var initialisation) = Declaration <$> newVar var <*>
    case initialisation of
      Just initialisation' -> Just <$> resolve initialisation'
      Nothing              -> pure Nothing

  checkLabels :: Declaration -> SemanticAnalyzerMonad Declaration
  checkLabels (Declaration var initialisation) = Declaration var <$>
    case initialisation of
      Just initialisation' -> Just <$> checkLabels initialisation'
      Nothing              -> pure Nothing

instance SemanticAnalyzer Exp where
  resolve :: Exp -> SemanticAnalyzerMonad Exp
  resolve e@(Constant _) = pure e
  resolve (Var var)      = Var <$> getVar var
  resolve (Unary op@(UnaryAssignmentOperator _) var@(Var _)) = Unary op <$> resolve var
  resolve (Unary    (UnaryAssignmentOperator _) e)           = throwError $ InvalidLHS e
  resolve (Unary op                             e)           = Unary op <$> resolve e
  resolve (Binary op exp1 exp2) = Binary op <$> resolve exp1 <*> resolve exp2
  resolve (Assignment var@(Var _) op rhs) = Assignment <$> resolve var <*> pure op <*> resolve rhs
  resolve (Assignment lhs         _  _)   = throwError $ InvalidLHS lhs
  resolve (Conditional cond conse altern) = Conditional <$> resolve cond <*> resolve conse <*> resolve altern

  checkLabels :: Exp -> SemanticAnalyzerMonad Exp
  checkLabels e@(Constant _) = pure e
  checkLabels (Var var)      = pure $ Var var
  checkLabels (Unary op@(UnaryAssignmentOperator _) var@(Var _)) = Unary op <$> checkLabels var
  checkLabels (Unary    (UnaryAssignmentOperator _) e)           = throwError $ InvalidLHS e
  checkLabels (Unary op                             e)           = Unary op <$> checkLabels e
  checkLabels (Binary op exp1 exp2) = Binary op <$> checkLabels exp1 <*> checkLabels exp2
  checkLabels (Assignment var@(Var _) op rhs) = Assignment <$> checkLabels var <*> pure op <*> checkLabels rhs
  checkLabels (Assignment lhs         _  _)   = throwError $ InvalidLHS lhs
  checkLabels (Conditional cond conse altern) = Conditional <$> checkLabels cond <*> checkLabels conse <*> checkLabels altern

