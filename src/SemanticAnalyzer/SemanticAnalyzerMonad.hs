{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module SemanticAnalyzer.SemanticAnalyzerMonad where

import           Parser.AST             (Identifier, UnaryOperator (..))
import qualified Parser.AST             as P
import           SemanticAnalyzer.AST
import           SemanticAnalyzer.Error (SemanticError (..))

import           Control.Applicative    (asum)
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
                               , envCurrentLoopLabel   :: Maybe Label
                               }

emptyEnvironment :: Environment
emptyEnvironment = Environment { envFunctionName       = Nothing
                               , envVarCounter         = 0
                               , envCurrentScopeVarEnv = emptyIdentifierMap
                               , envUpperScopesVarEnvs = []
                               , envLabelCounter       = 0
                               , envLabelEnv           = emptyIdentifierMap
                               , envCurrentLoopLabel   = Nothing
                               }

type SemanticAnalyzerMonad a = StateT Environment (Except SemanticError) a

resolveVar :: Identifier -> SemanticAnalyzerMonad Identifier
resolveVar var =
  do varCounter <- gets envVarCounter
     varEnv     <- gets envCurrentScopeVarEnv
     if var `M.member` varEnv then
       throwError $ DuplicateVariableDeclaration var
     else do
       let varCounter' = succ varCounter
       let var' = "var." <> var <> "." <> T.pack (show varCounter')
       let varEnv' = M.insert var var' varEnv
       modify (\env -> env { envCurrentScopeVarEnv = varEnv', envVarCounter = varCounter' })
       return var'

resolveLabel :: Identifier -> SemanticAnalyzerMonad Identifier
resolveLabel label =
  do functionName <- gets envFunctionName
     labelCounter <- gets envLabelCounter
     labelEnv     <- gets envLabelEnv
     if label `M.member` labelEnv then
       throwError $ DuplicateLabelDeclaration label
     else do
       let labelCounter' = succ labelCounter
       let label' = ".L" <> fromMaybe "" functionName <> "." <> label <> "." <> T.pack (show labelCounter')
       let labelEnv' = M.insert label label' labelEnv
       modify (\env -> env { envLabelEnv = labelEnv', envLabelCounter = labelCounter' })
       return label'

newLabel :: Identifier -> SemanticAnalyzerMonad Identifier
newLabel caption =
  do functionName <- gets envFunctionName
     labelCounter <- gets envLabelCounter
     let labelCounter' = succ labelCounter
     let label' = ".L" <> fromMaybe "" functionName <> "." <> caption <> "." <> T.pack (show labelCounter')
     modify (\env -> env { envLabelCounter = labelCounter' })
     return label'

getVar :: Identifier -> SemanticAnalyzerMonad Identifier
getVar var = do currentScopeVarEnv <- gets envCurrentScopeVarEnv
                upperScopeVarEnvs  <- gets envUpperScopesVarEnvs
                case asum $ map (M.lookup var) $ currentScopeVarEnv : upperScopeVarEnvs of
                  Nothing   -> throwError $ UndefinedVariableUse var
                  Just var' -> return var'

getLabel :: Identifier -> SemanticAnalyzerMonad Identifier
getLabel label = do m <- gets envLabelEnv
                    case M.lookup label m of
                      Nothing     -> throwError $ UndefinedLabelUse label
                      Just label' -> return label'

(<$?>) :: Applicative f => (t -> f a) -> Maybe t -> f (Maybe a)
f <$?> x = case x of
  Just x' -> Just <$> f x'
  Nothing -> pure Nothing

class VariableResolver a where
  resolveVariable :: a -> SemanticAnalyzerMonad a

class LabelResolver a where
  resolveLabelDeclaration :: a -> SemanticAnalyzerMonad a
  resolveLabelReference :: a -> SemanticAnalyzerMonad a

class StatementLabeler a b | a -> b where
  labelStatement :: a -> SemanticAnalyzerMonad b

instance VariableResolver P.Program where
  resolveVariable :: P.Program -> SemanticAnalyzerMonad P.Program
  resolveVariable (P.Program func) = P.Program <$> resolveVariable func

instance LabelResolver P.Program where
  resolveLabelDeclaration :: P.Program -> SemanticAnalyzerMonad P.Program
  resolveLabelDeclaration (P.Program func) = P.Program <$> resolveLabelDeclaration func

  resolveLabelReference :: P.Program -> SemanticAnalyzerMonad P.Program
  resolveLabelReference (P.Program func) = P.Program <$> resolveLabelReference func

instance StatementLabeler P.Program Program where
  labelStatement :: P.Program -> SemanticAnalyzerMonad Program
  labelStatement (P.Program func) = Program <$> labelStatement func

instance VariableResolver P.FunctionDefinition where
  resolveVariable :: P.FunctionDefinition -> SemanticAnalyzerMonad P.FunctionDefinition
  resolveVariable (P.Function name body) =
    do modify (\env -> env { envFunctionName = Just name })
       P.Function name <$> resolveVariable body

instance LabelResolver P.FunctionDefinition where
  resolveLabelDeclaration :: P.FunctionDefinition -> SemanticAnalyzerMonad P.FunctionDefinition
  resolveLabelDeclaration (P.Function name body) =
    modify (\env -> env { envFunctionName = Just name }) >> P.Function name <$> resolveLabelDeclaration body

  resolveLabelReference :: P.FunctionDefinition -> SemanticAnalyzerMonad P.FunctionDefinition
  resolveLabelReference (P.Function name body) =
    modify (\env -> env { envFunctionName = Just name }) >> P.Function name <$> resolveLabelReference body

instance StatementLabeler P.FunctionDefinition FunctionDefinition where
  labelStatement :: P.FunctionDefinition -> SemanticAnalyzerMonad FunctionDefinition
  labelStatement (P.Function name body) =
    modify (\env -> env { envFunctionName = Just name }) >> Function name <$> labelStatement body

instance VariableResolver P.Block where
  resolveVariable :: P.Block -> SemanticAnalyzerMonad P.Block
  resolveVariable (P.Block blockItens) =
    do currentScopeVarEnv <- gets envCurrentScopeVarEnv
       upperScopesVarEnvs <- gets envUpperScopesVarEnvs
       modify (\env -> env { envCurrentScopeVarEnv = emptyIdentifierMap, envUpperScopesVarEnvs = currentScopeVarEnv : upperScopesVarEnvs })
       blockItens' <- mapM resolveVariable blockItens
       modify (\env -> env { envCurrentScopeVarEnv = currentScopeVarEnv, envUpperScopesVarEnvs = upperScopesVarEnvs })
       pure $ P.Block blockItens'

instance LabelResolver P.Block where
  resolveLabelDeclaration :: P.Block -> SemanticAnalyzerMonad P.Block
  resolveLabelDeclaration (P.Block blockItens) = P.Block <$> mapM resolveLabelDeclaration blockItens

  resolveLabelReference :: P.Block -> SemanticAnalyzerMonad P.Block
  resolveLabelReference (P.Block blockItens) = P.Block <$> mapM resolveLabelReference blockItens

instance StatementLabeler P.Block Block where
  labelStatement :: P.Block -> SemanticAnalyzerMonad Block
  labelStatement (P.Block blockItens) = Block <$> mapM labelStatement blockItens

instance VariableResolver P.BlockItem where
  resolveVariable :: P.BlockItem -> SemanticAnalyzerMonad P.BlockItem
  resolveVariable (P.Stmt stmt) = P.Stmt <$> resolveVariable stmt
  resolveVariable (P.Dec  dec)  = P.Dec  <$> resolveVariable dec

instance LabelResolver P.BlockItem where
  resolveLabelDeclaration :: P.BlockItem -> SemanticAnalyzerMonad P.BlockItem
  resolveLabelDeclaration (P.Stmt stmt) = P.Stmt <$> resolveLabelDeclaration stmt
  resolveLabelDeclaration (P.Dec dec)   = P.Dec  <$> resolveLabelDeclaration dec

  resolveLabelReference :: P.BlockItem -> SemanticAnalyzerMonad P.BlockItem
  resolveLabelReference (P.Stmt stmt) = P.Stmt <$> resolveLabelReference stmt
  resolveLabelReference (P.Dec dec)   = P.Dec  <$> resolveLabelReference dec

instance StatementLabeler P.BlockItem BlockItem where
  labelStatement :: P.BlockItem -> SemanticAnalyzerMonad BlockItem
  labelStatement (P.Stmt stmt) = Stmt <$> labelStatement stmt
  labelStatement (P.Dec decl)  = Dec  <$> labelStatement decl

instance VariableResolver P.Statement where
  resolveVariable :: P.Statement -> SemanticAnalyzerMonad P.Statement
  resolveVariable (P.Return expr)     = P.Return <$> resolveVariable expr
  resolveVariable (P.Expression expr) = P.Expression <$> resolveVariable expr
  resolveVariable (P.If cond conseq altern) = P.If <$> resolveVariable cond <*> resolveVariable conseq <*> (resolveVariable <$?> altern)
  resolveVariable (P.Compound block) = P.Compound <$> resolveVariable block
  resolveVariable P.Null = pure P.Null
  resolveVariable (P.Label label) = pure $ P.Label label
  resolveVariable (P.Goto label) = pure $ P.Goto label
  resolveVariable P.Break = pure P.Break
  resolveVariable P.Continue = pure P.Continue
  resolveVariable (P.While cond body) = P.While <$> resolveVariable cond <*> resolveVariable body
  resolveVariable (P.DoWhile body cond) = P.DoWhile <$> resolveVariable body <*> resolveVariable cond
  resolveVariable (P.For ini cond post body) =
    do currentScopeVarEnv <- gets envCurrentScopeVarEnv
       upperScopesVarEnvs <- gets envUpperScopesVarEnvs
       modify (\env -> env { envCurrentScopeVarEnv = emptyIdentifierMap, envUpperScopesVarEnvs = currentScopeVarEnv : upperScopesVarEnvs })
       stmt <- P.For <$> resolveVariable ini <*> (resolveVariable <$?> cond) <*> (resolveVariable <$?> post) <*> resolveVariable body
       modify (\env -> env { envCurrentScopeVarEnv = currentScopeVarEnv, envUpperScopesVarEnvs = upperScopesVarEnvs })
       pure stmt

instance LabelResolver P.Statement where
  resolveLabelDeclaration :: P.Statement -> SemanticAnalyzerMonad P.Statement
  resolveLabelDeclaration (P.Return expr)     = P.Return <$> resolveLabelDeclaration expr
  resolveLabelDeclaration (P.Expression expr) = P.Expression <$> resolveLabelDeclaration expr
  resolveLabelDeclaration (P.If cond conseq altern) = P.If <$> resolveLabelDeclaration cond <*> resolveLabelDeclaration conseq <*> (resolveLabelDeclaration <$?> altern)
  resolveLabelDeclaration (P.Compound block) = P.Compound <$> resolveLabelDeclaration block
  resolveLabelDeclaration P.Null = pure P.Null
  resolveLabelDeclaration (P.Label label) = P.Label <$> resolveLabel label
  resolveLabelDeclaration (P.Goto label) = pure $ P.Goto label
  resolveLabelDeclaration P.Break = pure P.Break
  resolveLabelDeclaration P.Continue = pure P.Continue
  resolveLabelDeclaration (P.While cond body) = P.While <$> resolveLabelDeclaration cond <*> resolveLabelDeclaration body
  resolveLabelDeclaration (P.DoWhile body cond) = P.DoWhile <$> resolveLabelDeclaration body <*> resolveLabelDeclaration cond
  resolveLabelDeclaration (P.For ini cond post body) = P.For <$> resolveLabelDeclaration ini <*> (resolveLabelDeclaration <$?> cond) <*> (resolveLabelDeclaration <$?> post) <*> resolveLabelDeclaration body

  resolveLabelReference :: P.Statement -> SemanticAnalyzerMonad P.Statement
  resolveLabelReference (P.Return expr)     = P.Return <$> resolveLabelReference expr
  resolveLabelReference (P.Expression expr) = P.Expression <$> resolveLabelReference expr
  resolveLabelReference (P.If cond conseq altern) = P.If <$> resolveLabelReference cond <*> resolveLabelReference conseq <*> (resolveLabelReference <$?> altern)
  resolveLabelReference (P.Compound block) = P.Compound <$> resolveLabelReference block
  resolveLabelReference P.Null = pure P.Null
  resolveLabelReference (P.Label label) = pure $ P.Label label
  resolveLabelReference (P.Goto label) = P.Goto <$> getLabel label
  resolveLabelReference P.Break = pure P.Break
  resolveLabelReference P.Continue = pure P.Continue
  resolveLabelReference (P.While cond body) = P.While <$> resolveLabelReference cond <*> resolveLabelReference body
  resolveLabelReference (P.DoWhile body cond) = P.DoWhile <$> resolveLabelReference body <*> resolveLabelReference cond
  resolveLabelReference (P.For ini cond post body) = P.For <$> resolveLabelReference ini <*> (resolveLabelReference <$?> cond) <*> (resolveLabelReference <$?> post) <*> resolveLabelReference body

instance StatementLabeler P.Statement Statement where
  labelStatement :: P.Statement -> SemanticAnalyzerMonad Statement
  labelStatement (P.Return expr) = Return <$> labelStatement expr
  labelStatement (P.Expression expr) = Expression <$> labelStatement expr
  labelStatement (P.If cond conseq altern) = If <$> labelStatement cond <*> labelStatement conseq <*> (labelStatement <$?> altern)
  labelStatement (P.Compound block) = Compound <$> labelStatement block
  labelStatement P.Null = pure Null
  labelStatement (P.Label label) = pure $ Label label
  labelStatement (P.Goto label) = pure $ Goto label
  labelStatement P.Break = gets envCurrentLoopLabel >>= \case
    Nothing    -> throwError BreakOutsideLoop
    Just label -> pure $ Break label
  labelStatement P.Continue = gets envCurrentLoopLabel >>= \case
    Nothing    -> throwError ContinueOutsideLoop
    Just label -> pure $ Continue label
  labelStatement (P.While cond body) =
    do currentLoopLabel <- gets envCurrentLoopLabel
       newLoopLabel <- newLabel "while"
       modify (\env -> env { envCurrentLoopLabel = Just newLoopLabel })
       stmt <- While <$> labelStatement cond <*> labelStatement body <*> pure newLoopLabel
       modify (\env -> env { envCurrentLoopLabel = currentLoopLabel })
       pure stmt
  labelStatement (P.DoWhile body cond) =
    do currentLoopLabel <- gets envCurrentLoopLabel
       newLoopLabel <- newLabel "doWhile"
       modify (\env -> env { envCurrentLoopLabel = Just newLoopLabel })
       stmt <- DoWhile <$> labelStatement body <*> labelStatement cond <*> pure newLoopLabel
       modify (\env -> env { envCurrentLoopLabel = currentLoopLabel })
       pure stmt
  labelStatement (P.For ini cond post body) =
    do currentLoopLabel <- gets envCurrentLoopLabel
       newLoopLabel <- newLabel "for"
       modify (\env -> env { envCurrentLoopLabel = Just newLoopLabel })
       stmt <- For <$> labelStatement ini <*> (labelStatement <$?> cond) <*> (labelStatement <$?> post) <*> labelStatement body <*> pure newLoopLabel
       modify (\env -> env { envCurrentLoopLabel = currentLoopLabel })
       pure stmt

instance VariableResolver P.ForInit where
  resolveVariable :: P.ForInit -> SemanticAnalyzerMonad P.ForInit
  resolveVariable (P.InitDecl decl) = P.InitDecl <$> resolveVariable decl
  resolveVariable (P.InitExp expr)  = P.InitExp <$> (resolveVariable <$?> expr)

instance LabelResolver P.ForInit where
  resolveLabelDeclaration :: P.ForInit -> SemanticAnalyzerMonad P.ForInit
  resolveLabelDeclaration (P.InitDecl decl) = P.InitDecl <$> resolveLabelDeclaration decl
  resolveLabelDeclaration (P.InitExp expr)  = P.InitExp <$> (resolveLabelDeclaration <$?> expr)

  resolveLabelReference :: P.ForInit -> SemanticAnalyzerMonad P.ForInit
  resolveLabelReference (P.InitDecl decl) = P.InitDecl <$> resolveLabelReference decl
  resolveLabelReference (P.InitExp expr)  = P.InitExp <$> (resolveLabelReference <$?> expr)

instance StatementLabeler P.ForInit ForInit where
  labelStatement :: P.ForInit -> SemanticAnalyzerMonad ForInit
  labelStatement (P.InitDecl decl) = InitDecl <$> labelStatement decl
  labelStatement (P.InitExp expr)  = InitExp <$> (labelStatement <$?> expr)

instance VariableResolver P.Declaration where
  resolveVariable :: P.Declaration -> SemanticAnalyzerMonad P.Declaration
  resolveVariable (P.Declaration var initialisation) = P.Declaration <$> resolveVar var <*> (resolveVariable <$?> initialisation)

instance LabelResolver P.Declaration where
  resolveLabelDeclaration :: P.Declaration -> SemanticAnalyzerMonad P.Declaration
  resolveLabelDeclaration (P.Declaration var initialisation) = P.Declaration var <$> (resolveLabelDeclaration <$?> initialisation)

  resolveLabelReference :: P.Declaration -> SemanticAnalyzerMonad P.Declaration
  resolveLabelReference (P.Declaration var initialisation) = P.Declaration var <$> (resolveLabelReference <$?> initialisation)

instance StatementLabeler P.Declaration Declaration where
  labelStatement :: P.Declaration -> SemanticAnalyzerMonad Declaration
  labelStatement (P.Declaration var initialisation) = Declaration var <$> (labelStatement <$?> initialisation)

instance VariableResolver P.Exp where
  resolveVariable :: P.Exp -> SemanticAnalyzerMonad P.Exp
  resolveVariable (P.Constant c) = pure $ P.Constant c
  resolveVariable (P.Var var)      = P.Var <$> getVar var
  resolveVariable (P.Unary op@(P.UnaryAssignmentOperator _) var@(P.Var _)) = P.Unary op <$> resolveVariable var
  resolveVariable (P.Unary    (P.UnaryAssignmentOperator _) e)           = throwError $ InvalidLHS e
  resolveVariable (P.Unary op                             e)           = P.Unary op <$> resolveVariable e
  resolveVariable (P.Binary op exp1 exp2) = P.Binary op <$> resolveVariable exp1 <*> resolveVariable exp2
  resolveVariable (P.Assignment var@(P.Var _) op rhs) = P.Assignment <$> resolveVariable var <*> pure op <*> resolveVariable rhs
  resolveVariable (P.Assignment lhs         _  _)   = throwError $ InvalidLHS lhs
  resolveVariable (P.Conditional cond conse altern) = P.Conditional <$> resolveVariable cond <*> resolveVariable conse <*> resolveVariable altern

instance LabelResolver P.Exp where
  resolveLabelDeclaration :: P.Exp -> SemanticAnalyzerMonad P.Exp
  resolveLabelDeclaration (P.Constant c) = pure $ P.Constant c
  resolveLabelDeclaration (P.Var var)      = pure $ P.Var var
  resolveLabelDeclaration (P.Unary op@(UnaryAssignmentOperator _) var) = P.Unary op <$> resolveLabelDeclaration var
  resolveLabelDeclaration (P.Unary op                             e)           = P.Unary op <$> resolveLabelDeclaration e
  resolveLabelDeclaration (P.Binary op exp1 exp2) = P.Binary op <$> resolveLabelDeclaration exp1 <*> resolveLabelDeclaration exp2
  resolveLabelDeclaration (P.Assignment var op rhs) = P.Assignment <$> resolveLabelDeclaration var <*> pure op <*> resolveLabelDeclaration rhs
  resolveLabelDeclaration (P.Conditional cond conse altern) = P.Conditional <$> resolveLabelDeclaration cond <*> resolveLabelDeclaration conse <*> resolveLabelDeclaration altern

  resolveLabelReference :: P.Exp -> SemanticAnalyzerMonad P.Exp
  resolveLabelReference (P.Constant c) = pure $ P.Constant c
  resolveLabelReference (P.Var var)      = pure $ P.Var var
  resolveLabelReference (P.Unary op@(UnaryAssignmentOperator _) var) = P.Unary op <$> resolveLabelReference var
  resolveLabelReference (P.Unary op                             e)           = P.Unary op <$> resolveLabelReference e
  resolveLabelReference (P.Binary op exp1 exp2) = P.Binary op <$> resolveLabelReference exp1 <*> resolveLabelReference exp2
  resolveLabelReference (P.Assignment var op rhs) = P.Assignment <$> resolveLabelReference var <*> pure op <*> resolveLabelReference rhs
  resolveLabelReference (P.Conditional cond conse altern) = P.Conditional <$> resolveLabelReference cond <*> resolveLabelReference conse <*> resolveLabelReference altern

instance StatementLabeler P.Exp Exp where
  labelStatement :: P.Exp -> SemanticAnalyzerMonad Exp
  labelStatement (P.Constant c) = pure $ Constant c
  labelStatement (P.Var var)      = pure $ Var var
  labelStatement (P.Unary op@(UnaryAssignmentOperator _) var) = Unary op <$> labelStatement var
  labelStatement (P.Unary op                             e)           = Unary op <$> labelStatement e
  labelStatement (P.Binary op exp1 exp2) = Binary op <$> labelStatement exp1 <*> labelStatement exp2
  labelStatement (P.Assignment var op rhs) = Assignment <$> labelStatement var <*> pure op <*> labelStatement rhs
  labelStatement (P.Conditional cond conse altern) = Conditional <$> labelStatement cond <*> labelStatement conse <*> labelStatement altern


