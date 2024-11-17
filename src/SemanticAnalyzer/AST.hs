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
import           Control.Monad.State    (StateT, get, modify, put)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Numeric.Natural        (Natural)

type IdentifierMap = Map Identifier Identifier
type IdentifierEnv = (Natural, IdentifierMap)

data Environment = Environment { varEnv   :: IdentifierEnv
                               , labelEnv :: IdentifierEnv
                               , envName  :: Maybe Identifier
                               }

type SemanticAnalyzerMonad a = StateT Environment (Except SemanticError) a

emptyEnvironment :: Environment
emptyEnvironment = Environment { varEnv   = (0, M.empty)
                               , labelEnv = (0, M.empty)
                               , envName  = Nothing
                               }

makeUniqueIdentifier :: Identifier -> SemanticAnalyzerMonad Natural -> Identifier -> SemanticAnalyzerMonad Identifier
makeUniqueIdentifier base nextNum identifier = nextNum >>= \idCount ->
  return $ base <> "." <> identifier <> "." <> T.pack (show idCount)

makeUniqueVar :: Identifier -> SemanticAnalyzerMonad Identifier
makeUniqueVar = makeUniqueIdentifier "var" nextVar
  where nextVar :: SemanticAnalyzerMonad Natural
        nextVar = get >>= \env@Environment { varEnv = (n, m) } ->
                    let n' = succ n in put (env { varEnv = (n', m) }) >> return n'

makeUniqueLabel :: Identifier -> SemanticAnalyzerMonad Identifier
makeUniqueLabel label = get >>= \env@Environment { labelEnv = (n, m), envName = envName' } ->
    let nextLabel :: SemanticAnalyzerMonad Natural
        nextLabel = put (env { labelEnv = (n', m) }) >> return n'
          where n' = succ n

        getBase :: Text
        getBase = case envName' of Just name' -> ".L" <> name'
                                   Nothing    -> ".L"
    in makeUniqueIdentifier getBase nextLabel label

newIdentifier :: (Identifier -> SemanticAnalyzerMonad Identifier)
  -> (Environment -> IdentifierEnv)
  -> (Environment -> IdentifierEnv -> Environment)
  -> Identifier
  -> SemanticAnalyzerMonad Identifier
newIdentifier makeUnique getFromEnv putToEnv identifier =
  do env <- get
     let (n, m) = getFromEnv env
     if identifier `M.member` m then
       throwError $ DuplicateIdentifierDeclaration identifier
     else do
       identifier' <- makeUnique identifier
       put $ putToEnv env (n, M.insert identifier identifier' m)
       return identifier'

newVar :: Identifier -> SemanticAnalyzerMonad Identifier
newVar = newIdentifier makeUniqueVar varEnv (\env varEnv' -> env {varEnv = varEnv'} )

newLabel :: Identifier -> SemanticAnalyzerMonad Identifier
newLabel = newIdentifier makeUniqueLabel labelEnv (\env labelEnv' -> env {labelEnv = labelEnv'} )

getVar :: Identifier -> SemanticAnalyzerMonad Identifier
getVar var = do env <- get
                let (_, m) = varEnv env
                case M.lookup var m of
                  Nothing   -> throwError $ UndefinedIdentifierUse var
                  Just var' -> return var'

getLabel :: Identifier -> SemanticAnalyzerMonad Identifier
getLabel label = do env <- get
                    let (_, m) = labelEnv env
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
    do modify (\env -> env { envName = Just name })
       Function name <$> resolve body

  checkLabels :: FunctionDefinition -> SemanticAnalyzerMonad FunctionDefinition
  checkLabels (Function name body) =
    do modify (\env -> env { envName = Just name })
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
  checkLabels (Declaration var initialisation) = Declaration <$> newVar var <*>
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
  checkLabels (Var var)      = Var <$> getVar var
  checkLabels (Unary op@(UnaryAssignmentOperator _) var@(Var _)) = Unary op <$> checkLabels var
  checkLabels (Unary    (UnaryAssignmentOperator _) e)           = throwError $ InvalidLHS e
  checkLabels (Unary op                             e)           = Unary op <$> checkLabels e
  checkLabels (Binary op exp1 exp2) = Binary op <$> checkLabels exp1 <*> checkLabels exp2
  checkLabels (Assignment var@(Var _) op rhs) = Assignment <$> checkLabels var <*> pure op <*> checkLabels rhs
  checkLabels (Assignment lhs         _  _)   = throwError $ InvalidLHS lhs
  checkLabels (Conditional cond conse altern) = Conditional <$> checkLabels cond <*> checkLabels conse <*> checkLabels altern

