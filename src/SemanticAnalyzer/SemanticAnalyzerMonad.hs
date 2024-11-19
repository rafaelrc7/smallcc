{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module SemanticAnalyzer.SemanticAnalyzerMonad where

import           Parser.AST             (Block (..), BlockItem (..),
                                         Declaration (..), Exp (..),
                                         ForInit (..), FunctionDefinition (..),
                                         Identifier, Label (..), Program (..),
                                         Statement (..),
                                         UnaryOperator (UnaryAssignmentOperator),
                                         UnlabeledStatement (..), XAssignment,
                                         XBinary, XBreak, XCase, XCompound,
                                         XConditional, XConstant, XContinue,
                                         XDeclaration, XDefault, XDoWhile,
                                         XExpression, XFor, XGoto, XIf, XLabel,
                                         XNull, XReturn, XSwitch, XUnary, XVar,
                                         XWhile)
import qualified Parser.AST             as P
import           SemanticAnalyzer.Error (SemanticError (..))

import           Control.Applicative    (asum)
import           Control.Monad.Except   (Except, MonadError (..))
import           Control.Monad.State    (StateT, gets, modify)
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import           Numeric.Natural        (Natural)
import           Parser.ParserMonad     (ParserPhase)

data VariableResolvingPhase
data LabelResolvingPhase

type instance XExpression  VariableResolvingPhase = ()
type instance XCompound    VariableResolvingPhase = ()
type instance XIf          VariableResolvingPhase = ()
type instance XSwitch      VariableResolvingPhase = ()
type instance XWhile       VariableResolvingPhase = ()
type instance XDoWhile     VariableResolvingPhase = ()
type instance XFor         VariableResolvingPhase = ()
type instance XGoto        VariableResolvingPhase = ()
type instance XContinue    VariableResolvingPhase = ()
type instance XBreak       VariableResolvingPhase = ()
type instance XReturn      VariableResolvingPhase = ()
type instance XNull        VariableResolvingPhase = ()
type instance XLabel       VariableResolvingPhase = ()
type instance XCase        VariableResolvingPhase = ()
type instance XDefault     VariableResolvingPhase = ()
type instance XConstant    VariableResolvingPhase = ()
type instance XVar         VariableResolvingPhase = ()
type instance XUnary       VariableResolvingPhase = ()
type instance XBinary      VariableResolvingPhase = ()
type instance XAssignment  VariableResolvingPhase = ()
type instance XConditional VariableResolvingPhase = ()
type instance XDeclaration VariableResolvingPhase = ()

type instance XExpression  LabelResolvingPhase = ()
type instance XCompound    LabelResolvingPhase = ()
type instance XIf          LabelResolvingPhase = ()
type instance XSwitch      LabelResolvingPhase = ()
type instance XWhile       LabelResolvingPhase = Identifier
type instance XDoWhile     LabelResolvingPhase = Identifier
type instance XFor         LabelResolvingPhase = Identifier
type instance XGoto        LabelResolvingPhase = ()
type instance XContinue    LabelResolvingPhase = Identifier
type instance XBreak       LabelResolvingPhase = Identifier
type instance XReturn      LabelResolvingPhase = ()
type instance XNull        LabelResolvingPhase = ()
type instance XLabel       LabelResolvingPhase = ()
type instance XCase        LabelResolvingPhase = ()
type instance XDefault     LabelResolvingPhase = ()
type instance XConstant    LabelResolvingPhase = ()
type instance XVar         LabelResolvingPhase = ()
type instance XUnary       LabelResolvingPhase = ()
type instance XBinary      LabelResolvingPhase = ()
type instance XAssignment  LabelResolvingPhase = ()
type instance XConditional LabelResolvingPhase = ()
type instance XDeclaration LabelResolvingPhase = ()

type IdentifierMap = Map Identifier Identifier

emptyIdentifierMap :: IdentifierMap
emptyIdentifierMap = M.empty

data EnclosingLabel = LLoop   Identifier
                    | LSwitch Identifier

data Environment = Environment { envFunctionName       :: Maybe Identifier
                               , envVarCounter         :: Natural
                               , envCurrentScopeVarEnv :: IdentifierMap
                               , envUpperScopesVarEnvs :: [IdentifierMap]
                               , envLabelCounter       :: Natural
                               , envLabelEnv           :: IdentifierMap
                               , envEnclosingLabels    :: [EnclosingLabel]
                               }

emptyEnvironment :: Environment
emptyEnvironment = Environment { envFunctionName       = Nothing
                               , envVarCounter         = 0
                               , envCurrentScopeVarEnv = emptyIdentifierMap
                               , envUpperScopesVarEnvs = []
                               , envLabelCounter       = 0
                               , envLabelEnv           = emptyIdentifierMap
                               , envEnclosingLabels    = []
                               }

type SemanticAnalyzerMonad p a = StateT Environment (Except (SemanticError p)) a

-- type VariableResolverMonad a = SemanticAnalyzerMonad VariableResolvingPhase a
-- type SemanticAnalyzerMonad LabelResolvingPhase a = SemanticAnalyzerMonad LabelResolvingPhase a

resolveVar :: Identifier -> SemanticAnalyzerMonad VariableResolvingPhase Identifier
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

resolveLabel :: Identifier -> SemanticAnalyzerMonad VariableResolvingPhase Identifier
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

newLabel :: Identifier -> SemanticAnalyzerMonad VariableResolvingPhase Identifier
newLabel caption =
  do functionName <- gets envFunctionName
     labelCounter <- gets envLabelCounter
     let labelCounter' = succ labelCounter
     let label' = ".L" <> fromMaybe "" functionName <> "." <> caption <> "." <> T.pack (show labelCounter')
     modify (\env -> env { envLabelCounter = labelCounter' })
     return label'

getVar :: Identifier -> SemanticAnalyzerMonad VariableResolvingPhase Identifier
getVar var = do currentScopeVarEnv <- gets envCurrentScopeVarEnv
                upperScopeVarEnvs  <- gets envUpperScopesVarEnvs
                case asum $ map (M.lookup var) $ currentScopeVarEnv : upperScopeVarEnvs of
                  Nothing   -> throwError $ UndefinedVariableUse var
                  Just var' -> return var'

getLabel :: Identifier -> SemanticAnalyzerMonad VariableResolvingPhase Identifier
getLabel label = do m <- gets envLabelEnv
                    case M.lookup label m of
                      Nothing     -> throwError $ UndefinedLabelUse label
                      Just label' -> return label'

localState :: (Environment -> Environment) -> (Environment -> Environment) -> SemanticAnalyzerMonad p a -> SemanticAnalyzerMonad p a
localState push pop action =
  do modify push
     result <- action
     modify pop
     return result

withNewVarScope :: SemanticAnalyzerMonad p a -> SemanticAnalyzerMonad p a
withNewVarScope action =
    do currentScopeVarEnv <- gets envCurrentScopeVarEnv
       upperScopesVarEnvs <- gets envUpperScopesVarEnvs
       localState
         (\env -> env { envCurrentScopeVarEnv = emptyIdentifierMap, envUpperScopesVarEnvs = currentScopeVarEnv : upperScopesVarEnvs })
         (\env -> env { envCurrentScopeVarEnv = currentScopeVarEnv, envUpperScopesVarEnvs = upperScopesVarEnvs })
         action

withEnclosingLabel :: EnclosingLabel -> SemanticAnalyzerMonad p a -> SemanticAnalyzerMonad p a
withEnclosingLabel label action =
  do enclosingLabels <- gets envEnclosingLabels
     localState
       (\env -> env { envEnclosingLabels = label : enclosingLabels } )
       (\env -> env { envEnclosingLabels = enclosingLabels } )
       action

withEnclosingLoop :: Identifier -> SemanticAnalyzerMonad p a -> SemanticAnalyzerMonad p a
withEnclosingLoop = withEnclosingLabel . LLoop

withEnclosingSwitch :: Identifier -> SemanticAnalyzerMonad p a -> SemanticAnalyzerMonad p a
withEnclosingSwitch = withEnclosingLabel . LSwitch

getEnclosingLoopLabel :: SemanticAnalyzerMonad p Identifier
getEnclosingLoopLabel = gets envEnclosingLabels >>= getEnclosingLoopLabel'
  where getEnclosingLoopLabel' :: [EnclosingLabel] -> SemanticAnalyzerMonad p Identifier
        getEnclosingLoopLabel' []              = throwError StatementOutsideLoop
        getEnclosingLoopLabel' (LLoop label:_) = return label
        getEnclosingLoopLabel' (_:ls)          = getEnclosingLoopLabel' ls


getEnclosingSwitchLabel :: SemanticAnalyzerMonad p Identifier
getEnclosingSwitchLabel = gets envEnclosingLabels >>= getEnclosingSwitchLabel'
  where getEnclosingSwitchLabel' :: [EnclosingLabel] -> SemanticAnalyzerMonad p Identifier
        getEnclosingSwitchLabel' []                = throwError StatementOutsideSwitch
        getEnclosingSwitchLabel' (LSwitch label:_) = return label
        getEnclosingSwitchLabel' (_:ls)            = getEnclosingSwitchLabel' ls

getEnclosingLabel :: SemanticAnalyzerMonad p Identifier
getEnclosingLabel =
  do enclosingLabels <- gets envEnclosingLabels
     case enclosingLabels of
       (LLoop   label:_) -> return label
       (LSwitch label:_) -> return label
       []                -> throwError StatementOutsideSwitchOrLoop


(<$?>) :: Applicative f => (t -> f a) -> Maybe t -> f (Maybe a)
f <$?> x = case x of
  Just x' -> Just <$> f x'
  Nothing -> pure Nothing

class VariableResolver a where
  resolveVariable :: a ParserPhase -> SemanticAnalyzerMonad VariableResolvingPhase (a VariableResolvingPhase)

class LabelResolver a where
  resolveLabelDeclaration :: a VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (a VariableResolvingPhase)
  resolveLabelReference :: a VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (a LabelResolvingPhase)


instance VariableResolver Program where
  resolveVariable :: Program ParserPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Program VariableResolvingPhase)
  resolveVariable (Program func) = Program <$> resolveVariable func

instance VariableResolver FunctionDefinition where
  resolveVariable :: FunctionDefinition ParserPhase -> SemanticAnalyzerMonad VariableResolvingPhase (FunctionDefinition VariableResolvingPhase)
  resolveVariable (P.Function name body) = modify (\env -> env { envFunctionName = Just name }) >> Function name <$> resolveVariable body

instance VariableResolver Block where
  resolveVariable :: Block ParserPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Block VariableResolvingPhase)
  resolveVariable (Block blockItens) = withNewVarScope $ Block <$> mapM resolveVariable blockItens

instance VariableResolver BlockItem where
  resolveVariable :: BlockItem ParserPhase -> SemanticAnalyzerMonad VariableResolvingPhase (BlockItem VariableResolvingPhase)
  resolveVariable (BlockStatement   stmt) = BlockStatement   <$> resolveVariable stmt
  resolveVariable (BlockDeclaration decl) = BlockDeclaration <$> resolveVariable decl

instance VariableResolver Declaration where
  resolveVariable :: Declaration ParserPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Declaration VariableResolvingPhase)
  resolveVariable (Declaration () var initialisation) = Declaration () <$> resolveVar var <*> (resolveVariable <$?> initialisation)

instance VariableResolver Statement where
  resolveVariable :: Statement ParserPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Statement VariableResolvingPhase)
  resolveVariable (LabeledStatement label stmt) = LabeledStatement   <$> resolveVariable label <*> resolveVariable stmt
  resolveVariable (UnlabeledStatement stmt)     = UnlabeledStatement <$> resolveVariable stmt

instance VariableResolver UnlabeledStatement where
  resolveVariable :: UnlabeledStatement ParserPhase -> SemanticAnalyzerMonad VariableResolvingPhase (UnlabeledStatement VariableResolvingPhase)
  resolveVariable (Expression () expr)               = Expression () <$> resolveVariable expr
  resolveVariable (Compound   () block)              = Compound   () <$> resolveVariable block
  resolveVariable (If         () cond conseq altern) = If         () <$> resolveVariable cond <*> resolveVariable conseq <*> (resolveVariable <$?> altern)
  resolveVariable (Switch     () expr body)          = undefined -- TODO
  resolveVariable (While      () cond body)          = While      () <$> resolveVariable cond <*> resolveVariable body
  resolveVariable (DoWhile    () body cond)          = DoWhile    () <$> resolveVariable body <*> resolveVariable cond
  resolveVariable (For        () ini cond post body) = withNewVarScope $ For () <$> resolveVariable ini <*> (resolveVariable <$?> cond) <*> (resolveVariable <$?> post) <*> resolveVariable body
  resolveVariable (Goto       () label)              = pure $ Goto     () label
  resolveVariable (Continue   ())                    = pure $ Continue ()
  resolveVariable (Break      ())                    = pure $ Break    ()
  resolveVariable (Return     () expr)               = Return     () <$> resolveVariable expr
  resolveVariable (Null       ())                    = pure $ Null     ()

instance VariableResolver ForInit where
  resolveVariable :: ForInit ParserPhase -> SemanticAnalyzerMonad VariableResolvingPhase (ForInit VariableResolvingPhase)
  resolveVariable (InitDecl  decl) = InitDecl <$> resolveVariable decl
  resolveVariable (InitExp   expr) = InitExp  <$> (resolveVariable <$?> expr)

instance VariableResolver Label where
  resolveVariable :: Label ParserPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Label VariableResolvingPhase)
  resolveVariable (Label   () label) = pure $ Label   () label
  resolveVariable (Case    () expr)  = pure $ Case    () expr
  resolveVariable (Default ())       = pure $ Default ()

instance VariableResolver Exp where
  resolveVariable :: Exp ParserPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Exp VariableResolvingPhase)
  resolveVariable (Constant    () cons)                                         = pure $ Constant () cons
  resolveVariable (Var         () var)                                          = Var         ()    <$> getVar var
  resolveVariable (Unary       () op@(UnaryAssignmentOperator _) lhs@(Var _ _)) = Unary       () op <$> resolveVariable lhs
  resolveVariable (Unary       () (UnaryAssignmentOperator _)    lhs)           = resolveVariable lhs >>= throwError . InvalidLHS
  resolveVariable (Unary       () op expr)                                      = Unary       () op <$> resolveVariable expr
  resolveVariable (Binary      () op expr1 expr2)                               = Binary      () op <$> resolveVariable expr1 <*> resolveVariable expr2
  resolveVariable (Assignment  () lhs@(Var _ _) op rhs)                         = Assignment  ()    <$> resolveVariable lhs   <*> pure op                <*> resolveVariable rhs
  resolveVariable (Assignment  () lhs _ _)                                      = resolveVariable lhs >>= throwError . InvalidLHS
  resolveVariable (Conditional () cond conseq altern)                           = Conditional ()    <$> resolveVariable cond  <*> resolveVariable conseq <*> resolveVariable altern


instance LabelResolver Program where
  resolveLabelDeclaration :: Program VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Program VariableResolvingPhase)
  resolveLabelDeclaration (Program func) = Program <$> resolveLabelDeclaration func

  resolveLabelReference :: Program VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Program LabelResolvingPhase)
  resolveLabelReference (Program func) = Program <$> resolveLabelReference func

instance LabelResolver FunctionDefinition where
  resolveLabelDeclaration :: FunctionDefinition VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (FunctionDefinition VariableResolvingPhase)
  resolveLabelDeclaration (Function name body) = modify (\env -> env { envFunctionName = Just name }) >> Function name <$> resolveLabelDeclaration body

  resolveLabelReference :: FunctionDefinition VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (FunctionDefinition LabelResolvingPhase)
  resolveLabelReference (Function name body) = modify (\env -> env { envFunctionName = Just name }) >> Function name <$> resolveLabelReference body

instance LabelResolver Block where
  resolveLabelDeclaration :: Block VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Block VariableResolvingPhase)
  resolveLabelDeclaration (Block blockItens) = Block <$> mapM resolveLabelDeclaration blockItens

  resolveLabelReference :: Block VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Block LabelResolvingPhase)
  resolveLabelReference (Block blockItens) = Block <$> mapM resolveLabelReference blockItens

instance LabelResolver BlockItem where
  resolveLabelDeclaration :: BlockItem VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (BlockItem VariableResolvingPhase)
  resolveLabelDeclaration (BlockStatement   stmt) = BlockStatement   <$> resolveLabelDeclaration stmt
  resolveLabelDeclaration (BlockDeclaration decl) = BlockDeclaration <$> resolveLabelDeclaration decl

  resolveLabelReference :: BlockItem VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (BlockItem LabelResolvingPhase)
  resolveLabelReference (BlockStatement   stmt) = BlockStatement   <$> resolveLabelReference stmt
  resolveLabelReference (BlockDeclaration decl) = BlockDeclaration <$> resolveLabelReference decl

instance LabelResolver Declaration where
  resolveLabelDeclaration :: Declaration VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Declaration VariableResolvingPhase)
  resolveLabelDeclaration (Declaration () var initialisation) = Declaration () var <$> (resolveLabelDeclaration <$?> initialisation)

  resolveLabelReference :: Declaration VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Declaration LabelResolvingPhase)
  resolveLabelReference (Declaration () var initialisation) = Declaration () var <$> (resolveLabelReference <$?> initialisation)

instance LabelResolver Statement where
  resolveLabelDeclaration :: Statement VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Statement VariableResolvingPhase)
  resolveLabelDeclaration (LabeledStatement label stmt) = LabeledStatement   <$> resolveLabelDeclaration label <*> resolveLabelDeclaration stmt
  resolveLabelDeclaration (UnlabeledStatement stmt)     = UnlabeledStatement <$> resolveLabelDeclaration stmt

  resolveLabelReference :: Statement VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Statement LabelResolvingPhase)
  resolveLabelReference (LabeledStatement label stmt) = LabeledStatement   <$> resolveLabelReference label <*> resolveLabelReference stmt
  resolveLabelReference (UnlabeledStatement stmt)     = UnlabeledStatement <$> resolveLabelReference stmt

instance LabelResolver UnlabeledStatement where
  resolveLabelDeclaration :: UnlabeledStatement VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (UnlabeledStatement VariableResolvingPhase)
  resolveLabelDeclaration (Expression () expr)               = Expression () <$> resolveLabelDeclaration expr
  resolveLabelDeclaration (Compound   () block)              = Compound   () <$> resolveLabelDeclaration block
  resolveLabelDeclaration (If         () cond conseq altern) = If         () <$> resolveLabelDeclaration cond <*> resolveLabelDeclaration conseq <*> (resolveLabelDeclaration <$?> altern)
  resolveLabelDeclaration (Switch     () expr body)          = undefined -- TODO
  resolveLabelDeclaration (While      () cond body)          = While      () <$> resolveLabelDeclaration cond <*> resolveLabelDeclaration body
  resolveLabelDeclaration (DoWhile    () body cond)          = DoWhile    () <$> resolveLabelDeclaration body <*> resolveLabelDeclaration cond
  resolveLabelDeclaration (For        () ini cond post body) = For        () <$> resolveLabelDeclaration ini  <*> (resolveLabelDeclaration <$?> cond) <*> (resolveLabelDeclaration <$?> post) <*> resolveLabelDeclaration body
  resolveLabelDeclaration (Goto       () label)              = pure $ Goto     () label
  resolveLabelDeclaration (Continue   ())                    = pure $ Continue ()
  resolveLabelDeclaration (Break      ())                    = pure $ Break    ()
  resolveLabelDeclaration (Return     () expr)               = Return     () <$> resolveLabelDeclaration expr
  resolveLabelDeclaration (Null       ())                    = pure $ Null     ()

  resolveLabelReference :: UnlabeledStatement VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (UnlabeledStatement LabelResolvingPhase)
  resolveLabelReference (Goto       () label)              = Goto       () <$> getLabel label
  resolveLabelReference (Return     () expr)               = Return     () <$> resolveLabelReference expr
  resolveLabelReference (Expression () expr)               = Expression () <$> resolveLabelReference expr
  resolveLabelReference (Compound   () block)              = Compound   () <$> resolveLabelReference block
  resolveLabelReference (If         () cond conseq altern) = If         () <$> resolveLabelReference cond <*> resolveLabelReference conseq <*> (resolveLabelReference <$?> altern)
  resolveLabelReference (Switch     () expr body)          = undefined -- TODO
  resolveLabelReference (Continue   ())                    = Continue <$> getEnclosingLoopLabel
  resolveLabelReference (Break      ())                    = Break    <$> getEnclosingLabel
  resolveLabelReference (Null       ())                    = pure $ Null     ()
  resolveLabelReference (While      () cond body)          =
    do loopLabel <- newLabel "while"
       withEnclosingLoop loopLabel $ While loopLabel <$> resolveLabelReference cond <*> resolveLabelReference body
  resolveLabelReference (DoWhile    () body cond)          =
    do loopLabel <- newLabel "doWhile"
       withEnclosingLoop loopLabel $ DoWhile loopLabel <$> resolveLabelReference body <*> resolveLabelReference cond
  resolveLabelReference (For        () ini cond post body) =
    do loopLabel <- newLabel "for"
       withEnclosingLoop loopLabel $ For loopLabel <$> resolveLabelReference ini <*> (resolveLabelReference <$?> cond) <*> (resolveLabelReference <$?> post) <*> resolveLabelReference body

instance LabelResolver ForInit where
  resolveLabelDeclaration :: ForInit VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (ForInit VariableResolvingPhase)
  resolveLabelDeclaration (InitDecl decl) = InitDecl <$> resolveLabelDeclaration decl
  resolveLabelDeclaration (InitExp  expr) = InitExp  <$> (resolveLabelDeclaration <$?> expr)

  resolveLabelReference :: ForInit VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (ForInit LabelResolvingPhase)
  resolveLabelReference (InitDecl decl) = InitDecl <$> resolveLabelReference decl
  resolveLabelReference (InitExp  expr) = InitExp  <$> (resolveLabelReference <$?> expr)

instance LabelResolver Label where
  resolveLabelDeclaration :: Label VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Label VariableResolvingPhase)
  resolveLabelDeclaration (Label   () label) = Label () <$> resolveLabel label
  resolveLabelDeclaration (Case    () expr)  = pure $ Case    () expr
  resolveLabelDeclaration (Default ())       = pure $ Default ()

  resolveLabelReference :: Label VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Label LabelResolvingPhase)
  resolveLabelReference (Label   () label) = pure $ Label   () label
  resolveLabelReference (Case    () expr)  = pure $ Case    () expr
  resolveLabelReference (Default ())       = pure $ Default ()

instance LabelResolver Exp where
  resolveLabelDeclaration :: Exp VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Exp VariableResolvingPhase)
  resolveLabelDeclaration (Constant    () cons)               = pure $ Constant () cons
  resolveLabelDeclaration (Var         () var)                = pure $ Var      () var
  resolveLabelDeclaration (Unary       () op expr)            = Unary       () op <$> resolveLabelDeclaration expr
  resolveLabelDeclaration (Binary      () op expr1 expr2)     = Binary      () op <$> resolveLabelDeclaration expr1 <*> resolveLabelDeclaration expr2
  resolveLabelDeclaration (Assignment  () lhs op rhs)         = Assignment  ()    <$> resolveLabelDeclaration lhs   <*> pure op                        <*> resolveLabelDeclaration rhs
  resolveLabelDeclaration (Conditional () cond conseq altern) = Conditional ()    <$> resolveLabelDeclaration cond  <*> resolveLabelDeclaration conseq <*> resolveLabelDeclaration altern

  resolveLabelReference :: Exp VariableResolvingPhase -> SemanticAnalyzerMonad VariableResolvingPhase (Exp LabelResolvingPhase)
  resolveLabelReference (Constant    () cons)               = pure $ Constant () cons
  resolveLabelReference (Var         () var)                = pure $ Var      () var
  resolveLabelReference (Unary       () op expr)            = Unary       () op <$> resolveLabelReference expr
  resolveLabelReference (Binary      () op expr1 expr2)     = Binary      () op <$> resolveLabelReference expr1 <*> resolveLabelReference expr2
  resolveLabelReference (Assignment  () lhs op rhs)         = Assignment  ()    <$> resolveLabelReference lhs   <*> pure op                        <*> resolveLabelReference rhs
  resolveLabelReference (Conditional () cond conseq altern) = Conditional ()    <$> resolveLabelReference cond  <*> resolveLabelReference conseq <*> resolveLabelReference altern

