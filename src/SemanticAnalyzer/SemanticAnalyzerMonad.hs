{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module SemanticAnalyzer.SemanticAnalyzerMonad where

import           Parser.AST             (BinaryOperator (..), Block (..),
                                         BlockItem (..), Constant (..),
                                         Declaration (..), Exp (..),
                                         ForInit (..),
                                         FunctionDeclaration (FunctionDeclaration),
                                         Identifier, Label (..), Program (..),
                                         Statement (..), UnaryOperator (..),
                                         UnlabeledStatement (..),
                                         VarDeclaration (VarDeclaration),
                                         XAssignment, XBinary, XBreak, XCase,
                                         XCaseV, XCompound, XConditional,
                                         XConstant, XContinue, XDefault,
                                         XDoWhile, XExpression, XFor, XFunDecl,
                                         XGoto, XIf, XLabel, XNull, XReturn,
                                         XSwitch, XUnary, XVar, XVarDecl,
                                         XWhile)
import           SemanticAnalyzer.Error (SemanticError (..))

import           Control.Applicative    (asum)
import           Control.Monad.Except   (Except, MonadError (..))
import           Control.Monad.State    (StateT, gets, modify)
import           Data.Bits              (Bits (complement), shiftL, shiftR, xor,
                                         (.&.), (.|.))
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import           Numeric.Natural        (Natural)
import           Parser.ParserMonad     (ParserPhase)

data VariableResolvingPhase
data LabelResolvingPhase
data SwitchResolvingPhase

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
type instance XDefault     VariableResolvingPhase = ()
type instance XCase        VariableResolvingPhase = ()
type instance XCaseV       VariableResolvingPhase = Exp VariableResolvingPhase
type instance XConstant    VariableResolvingPhase = ()
type instance XVar         VariableResolvingPhase = ()
type instance XUnary       VariableResolvingPhase = ()
type instance XBinary      VariableResolvingPhase = ()
type instance XAssignment  VariableResolvingPhase = ()
type instance XConditional VariableResolvingPhase = ()
type instance XFunDecl     VariableResolvingPhase = ()
type instance XVarDecl     VariableResolvingPhase = ()

type instance XExpression  LabelResolvingPhase = ()
type instance XCompound    LabelResolvingPhase = ()
type instance XIf          LabelResolvingPhase = ()
type instance XSwitch      LabelResolvingPhase = Identifier
type instance XWhile       LabelResolvingPhase = Identifier
type instance XDoWhile     LabelResolvingPhase = Identifier
type instance XFor         LabelResolvingPhase = Identifier
type instance XGoto        LabelResolvingPhase = ()
type instance XContinue    LabelResolvingPhase = Identifier
type instance XBreak       LabelResolvingPhase = Identifier
type instance XReturn      LabelResolvingPhase = ()
type instance XNull        LabelResolvingPhase = ()
type instance XLabel       LabelResolvingPhase = ()
type instance XDefault     LabelResolvingPhase = Identifier
type instance XCase        LabelResolvingPhase = Identifier
type instance XCaseV       LabelResolvingPhase = Exp LabelResolvingPhase
type instance XConstant    LabelResolvingPhase = ()
type instance XVar         LabelResolvingPhase = ()
type instance XUnary       LabelResolvingPhase = ()
type instance XBinary      LabelResolvingPhase = ()
type instance XAssignment  LabelResolvingPhase = ()
type instance XConditional LabelResolvingPhase = ()
type instance XFunDecl     LabelResolvingPhase = ()
type instance XVarDecl     LabelResolvingPhase = ()

data SwitchLabel = SCase Constant
                 | SDefault
  deriving (Show, Eq, Ord)

type instance XExpression  SwitchResolvingPhase = ()
type instance XCompound    SwitchResolvingPhase = ()
type instance XIf          SwitchResolvingPhase = ()
type instance XSwitch      SwitchResolvingPhase = (Identifier, Map SwitchLabel Identifier)
type instance XWhile       SwitchResolvingPhase = Identifier
type instance XDoWhile     SwitchResolvingPhase = Identifier
type instance XFor         SwitchResolvingPhase = Identifier
type instance XGoto        SwitchResolvingPhase = ()
type instance XContinue    SwitchResolvingPhase = Identifier
type instance XBreak       SwitchResolvingPhase = Identifier
type instance XReturn      SwitchResolvingPhase = ()
type instance XNull        SwitchResolvingPhase = ()
type instance XLabel       SwitchResolvingPhase = ()
type instance XDefault     SwitchResolvingPhase = Identifier
type instance XCase        SwitchResolvingPhase = Identifier
type instance XCaseV       SwitchResolvingPhase = Constant
type instance XConstant    SwitchResolvingPhase = ()
type instance XVar         SwitchResolvingPhase = ()
type instance XUnary       SwitchResolvingPhase = ()
type instance XBinary      SwitchResolvingPhase = ()
type instance XAssignment  SwitchResolvingPhase = ()
type instance XConditional SwitchResolvingPhase = ()
type instance XFunDecl     SwitchResolvingPhase = ()
type instance XVarDecl     SwitchResolvingPhase = ()

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
                               , envSwitchLabels       :: Maybe (Map SwitchLabel Identifier)
                               }

emptyEnvironment :: Environment
emptyEnvironment = Environment { envFunctionName       = Nothing
                               , envVarCounter         = 0
                               , envCurrentScopeVarEnv = emptyIdentifierMap
                               , envUpperScopesVarEnvs = []
                               , envLabelCounter       = 0
                               , envLabelEnv           = emptyIdentifierMap
                               , envEnclosingLabels    = []
                               , envSwitchLabels       = Nothing
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

localState :: (Environment -> Environment) -> (Environment -> Environment) -> SemanticAnalyzerMonad a -> SemanticAnalyzerMonad a
localState push pop action =
  do modify push
     result <- action
     modify pop
     return result

withNewVarScope :: SemanticAnalyzerMonad a -> SemanticAnalyzerMonad a
withNewVarScope action =
    do currentScopeVarEnv <- gets envCurrentScopeVarEnv
       upperScopesVarEnvs <- gets envUpperScopesVarEnvs
       localState
         (\env -> env { envCurrentScopeVarEnv = emptyIdentifierMap, envUpperScopesVarEnvs = currentScopeVarEnv : upperScopesVarEnvs })
         (\env -> env { envCurrentScopeVarEnv = currentScopeVarEnv, envUpperScopesVarEnvs = upperScopesVarEnvs })
         action

withEnclosingLabel :: EnclosingLabel -> SemanticAnalyzerMonad a -> SemanticAnalyzerMonad a
withEnclosingLabel label action =
  do enclosingLabels <- gets envEnclosingLabels
     localState
       (\env -> env { envEnclosingLabels = label : enclosingLabels } )
       (\env -> env { envEnclosingLabels = enclosingLabels } )
       action

withEnclosingLoop :: Identifier -> SemanticAnalyzerMonad a -> SemanticAnalyzerMonad a
withEnclosingLoop = withEnclosingLabel . LLoop

withEnclosingSwitch :: Identifier -> SemanticAnalyzerMonad a -> SemanticAnalyzerMonad a
withEnclosingSwitch = withEnclosingLabel . LSwitch

withSwitchLabels :: SemanticAnalyzerMonad a -> SemanticAnalyzerMonad a
withSwitchLabels action =
  do switchLabels <- gets envSwitchLabels
     localState
       (\env -> env { envSwitchLabels = Just M.empty })
       (\env -> env { envSwitchLabels = switchLabels })
       action

getEnclosingLoopLabel :: SemanticAnalyzerMonad Identifier
getEnclosingLoopLabel = gets envEnclosingLabels >>= getEnclosingLoopLabel'
  where getEnclosingLoopLabel' :: [EnclosingLabel] -> SemanticAnalyzerMonad Identifier
        getEnclosingLoopLabel' []              = throwError StatementOutsideLoop
        getEnclosingLoopLabel' (LLoop label:_) = return label
        getEnclosingLoopLabel' (_:ls)          = getEnclosingLoopLabel' ls


getEnclosingSwitchLabel :: SemanticAnalyzerMonad Identifier
getEnclosingSwitchLabel = gets envEnclosingLabels >>= getEnclosingSwitchLabel'
  where getEnclosingSwitchLabel' :: [EnclosingLabel] -> SemanticAnalyzerMonad Identifier
        getEnclosingSwitchLabel' []                = throwError StatementOutsideSwitch
        getEnclosingSwitchLabel' (LSwitch label:_) = return label
        getEnclosingSwitchLabel' (_:ls)            = getEnclosingSwitchLabel' ls

getEnclosingLabel :: SemanticAnalyzerMonad Identifier
getEnclosingLabel =
  do enclosingLabels <- gets envEnclosingLabels
     case enclosingLabels of
       (LLoop   label:_) -> return label
       (LSwitch label:_) -> return label
       []                -> throwError StatementOutsideSwitchOrLoop

collapseConstantExp :: Exp p -> SemanticAnalyzerMonad Constant
collapseConstantExp (Unary       _ Complement expr)    = collapseConstantExp expr >>= \(CInt val) -> return . CInt . complement $ val
collapseConstantExp (Unary       _ Negate expr)        = collapseConstantExp expr >>= \(CInt val) -> return . CInt . negate $ val
collapseConstantExp (Unary       _ Not expr)           = collapseConstantExp expr >>= \(CInt val) -> return . CInt . boolToInt . not . intToBool $ val
collapseConstantExp (Binary      _ op lhs rhs)         = collapseConstantExp lhs >>= \lhs' -> collapseConstantExp rhs >>= \rhs' -> return $ intOverConst (getBinary op) lhs' rhs'
collapseConstantExp (Conditional _ cond conseq altern) = collapseConstantExp cond >>= \(CInt cond') -> if intToBool cond' then collapseConstantExp conseq else collapseConstantExp altern
collapseConstantExp (Constant    _ e@(CInt _))         = return e
collapseConstantExp (FunctionCall {})                  = throwError NotConstantExpression
collapseConstantExp (Unary        {})                  = throwError NotConstantExpression
collapseConstantExp (Var          {})                  = throwError NotConstantExpression
collapseConstantExp (Assignment   {})                  = throwError NotConstantExpression

intOverConst :: (Int -> Int -> Int) -> Constant -> Constant -> Constant
intOverConst op (CInt a) (CInt b) = CInt $ a `op` b

getBinary :: BinaryOperator -> (Int -> Int -> Int)
getBinary Add            = (+)
getBinary Subtract       = (-)
getBinary Multiply       = (*)
getBinary Divide         = div
getBinary Remainder      = rem
getBinary BitAnd         = (.&.)
getBinary BitOr          = (.|.)
getBinary BitXOR         = xor
getBinary BitShiftLeft   = shiftL
getBinary BitShiftRight  = shiftR
getBinary And            = \a b -> boolToInt $ intToBool a && intToBool b
getBinary Or             = \a b -> boolToInt $ intToBool a || intToBool b
getBinary EqualsTo       = \a b -> boolToInt $ a == b
getBinary NotEqualsTo    = \a b -> boolToInt $ a /= b
getBinary Less           = \a b -> boolToInt $ a < b
getBinary LessOrEqual    = \a b -> boolToInt $ a <= b
getBinary Greater        = \a b -> boolToInt $ a > b
getBinary GreaterOrEqual = \a b -> boolToInt $ a >= b

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

intToBool :: Int -> Bool
intToBool 0 = False
intToBool _ = True

(<$?>) :: Applicative f => (t -> f a) -> Maybe t -> f (Maybe a)
f <$?> x = case x of
  Just x' -> Just <$> f x'
  Nothing -> pure Nothing

class VariableResolver a where
  resolveVariable :: a ParserPhase -> SemanticAnalyzerMonad (a VariableResolvingPhase)

class LabelResolver a where
  resolveLabelDeclaration :: a VariableResolvingPhase -> SemanticAnalyzerMonad (a VariableResolvingPhase)
  resolveLabelReference :: a VariableResolvingPhase -> SemanticAnalyzerMonad (a LabelResolvingPhase)

class SwitchResolver a where
  resolveSwitch :: a LabelResolvingPhase -> SemanticAnalyzerMonad (a SwitchResolvingPhase)


instance VariableResolver Program where
  resolveVariable :: Program ParserPhase -> SemanticAnalyzerMonad (Program VariableResolvingPhase)
  resolveVariable (Program func) = Program <$> mapM resolveVariable func

instance VariableResolver Declaration where
  resolveVariable :: Declaration ParserPhase -> SemanticAnalyzerMonad (Declaration VariableResolvingPhase)
  resolveVariable (FunDecl decl) = FunDecl <$> resolveVariable decl
  resolveVariable (VarDecl decl) = VarDecl <$> resolveVariable decl

instance VariableResolver FunctionDeclaration where
  resolveVariable :: FunctionDeclaration ParserPhase -> SemanticAnalyzerMonad (FunctionDeclaration VariableResolvingPhase)
  resolveVariable (FunctionDeclaration _ name params body) = modify (\env -> env { envFunctionName = Just name }) >> FunctionDeclaration () name params <$> (resolveVariable <$?> body)

instance VariableResolver Block where
  resolveVariable :: Block ParserPhase -> SemanticAnalyzerMonad (Block VariableResolvingPhase)
  resolveVariable (Block blockItens) = withNewVarScope $ Block <$> mapM resolveVariable blockItens

instance VariableResolver BlockItem where
  resolveVariable :: BlockItem ParserPhase -> SemanticAnalyzerMonad (BlockItem VariableResolvingPhase)
  resolveVariable (BlockStatement   stmt) = BlockStatement   <$> resolveVariable stmt
  resolveVariable (BlockDeclaration decl) = BlockDeclaration <$> resolveVariable decl

instance VariableResolver VarDeclaration where
  resolveVariable :: VarDeclaration ParserPhase -> SemanticAnalyzerMonad (VarDeclaration VariableResolvingPhase)
  resolveVariable (VarDeclaration () var initialisation) = VarDeclaration () <$> resolveVar var <*> (resolveVariable <$?> initialisation)

instance VariableResolver Statement where
  resolveVariable :: Statement ParserPhase -> SemanticAnalyzerMonad (Statement VariableResolvingPhase)
  resolveVariable (LabeledStatement label stmt) = LabeledStatement   <$> resolveVariable label <*> resolveVariable stmt
  resolveVariable (UnlabeledStatement stmt)     = UnlabeledStatement <$> resolveVariable stmt

instance VariableResolver UnlabeledStatement where
  resolveVariable :: UnlabeledStatement ParserPhase -> SemanticAnalyzerMonad (UnlabeledStatement VariableResolvingPhase)
  resolveVariable (Expression () expr)               = Expression () <$> resolveVariable expr
  resolveVariable (Compound   () block)              = Compound   () <$> resolveVariable block
  resolveVariable (If         () cond conseq altern) = If         () <$> resolveVariable cond <*> resolveVariable conseq <*> (resolveVariable <$?> altern)
  resolveVariable (Switch     () expr body)          = Switch     () <$> resolveVariable expr <*> resolveVariable body
  resolveVariable (While      () cond body)          = While      () <$> resolveVariable cond <*> resolveVariable body
  resolveVariable (DoWhile    () body cond)          = DoWhile    () <$> resolveVariable body <*> resolveVariable cond
  resolveVariable (For        () ini cond post body) = withNewVarScope $ For () <$> resolveVariable ini <*> (resolveVariable <$?> cond) <*> (resolveVariable <$?> post) <*> resolveVariable body
  resolveVariable (Goto       () label)              = pure $ Goto     () label
  resolveVariable (Continue   ())                    = pure $ Continue ()
  resolveVariable (Break      ())                    = pure $ Break    ()
  resolveVariable (Return     () expr)               = Return     () <$> resolveVariable expr
  resolveVariable (Null       ())                    = pure $ Null     ()

instance VariableResolver ForInit where
  resolveVariable :: ForInit ParserPhase -> SemanticAnalyzerMonad (ForInit VariableResolvingPhase)
  resolveVariable (InitDecl  decl) = InitDecl <$> resolveVariable decl
  resolveVariable (InitExp   expr) = InitExp  <$> (resolveVariable <$?> expr)

instance VariableResolver Label where
  resolveVariable :: Label ParserPhase -> SemanticAnalyzerMonad (Label VariableResolvingPhase)
  resolveVariable (Case    () expr)  = Case () <$> resolveVariable expr
  resolveVariable (Label   () label) = pure $ Label   () label
  resolveVariable (Default ())       = pure $ Default ()

instance VariableResolver Exp where
  resolveVariable :: Exp ParserPhase -> SemanticAnalyzerMonad (Exp VariableResolvingPhase)
  resolveVariable (Constant    () cons)                                         = pure $ Constant () cons
  resolveVariable (Var         () var)                                          = Var         ()    <$> getVar var
  resolveVariable (Unary       () op@(UnaryAssignmentOperator _) lhs@(Var _ _)) = Unary       () op <$> resolveVariable lhs
  resolveVariable (Unary       () (UnaryAssignmentOperator _)    _)             = throwError InvalidLHS
  resolveVariable (Unary       () op expr)                                      = Unary       () op <$> resolveVariable expr
  resolveVariable (Binary      () op expr1 expr2)                               = Binary      () op <$> resolveVariable expr1 <*> resolveVariable expr2
  resolveVariable (Assignment  () lhs@(Var _ _) op rhs)                         = Assignment  ()    <$> resolveVariable lhs   <*> pure op                <*> resolveVariable rhs
  resolveVariable (Assignment  () _ _ _)                                        = throwError InvalidLHS
  resolveVariable (Conditional () cond conseq altern)                           = Conditional ()    <$> resolveVariable cond  <*> resolveVariable conseq <*> resolveVariable altern


instance LabelResolver Program where
  resolveLabelDeclaration :: Program VariableResolvingPhase -> SemanticAnalyzerMonad (Program VariableResolvingPhase)
  resolveLabelDeclaration (Program func) = Program <$> mapM resolveLabelDeclaration func

  resolveLabelReference :: Program VariableResolvingPhase -> SemanticAnalyzerMonad (Program LabelResolvingPhase)
  resolveLabelReference (Program func) = Program <$> mapM resolveLabelReference func

instance LabelResolver Declaration where
  resolveLabelDeclaration :: Declaration VariableResolvingPhase -> SemanticAnalyzerMonad (Declaration VariableResolvingPhase)
  resolveLabelDeclaration (FunDecl decl) = FunDecl <$> resolveLabelDeclaration decl
  resolveLabelDeclaration (VarDecl decl) = VarDecl <$> resolveLabelDeclaration decl

  resolveLabelReference :: Declaration VariableResolvingPhase -> SemanticAnalyzerMonad (Declaration LabelResolvingPhase)
  resolveLabelReference (FunDecl decl) = FunDecl <$> resolveLabelReference decl
  resolveLabelReference (VarDecl decl) = VarDecl <$> resolveLabelReference decl

instance LabelResolver FunctionDeclaration where
  resolveLabelDeclaration :: FunctionDeclaration VariableResolvingPhase -> SemanticAnalyzerMonad (FunctionDeclaration VariableResolvingPhase)
  resolveLabelDeclaration (FunctionDeclaration _ name params body) = modify (\env -> env { envFunctionName = Just name }) >> FunctionDeclaration () name params <$> (resolveLabelDeclaration <$?> body)

  resolveLabelReference :: FunctionDeclaration VariableResolvingPhase -> SemanticAnalyzerMonad (FunctionDeclaration LabelResolvingPhase)
  resolveLabelReference (FunctionDeclaration _ name params body) = modify (\env -> env { envFunctionName = Just name }) >> FunctionDeclaration () name params <$> resolveLabelReference <$?> body

instance LabelResolver Block where
  resolveLabelDeclaration :: Block VariableResolvingPhase -> SemanticAnalyzerMonad (Block VariableResolvingPhase)
  resolveLabelDeclaration (Block blockItens) = Block <$> mapM resolveLabelDeclaration blockItens

  resolveLabelReference :: Block VariableResolvingPhase -> SemanticAnalyzerMonad (Block LabelResolvingPhase)
  resolveLabelReference (Block blockItens) = Block <$> mapM resolveLabelReference blockItens

instance LabelResolver BlockItem where
  resolveLabelDeclaration :: BlockItem VariableResolvingPhase -> SemanticAnalyzerMonad (BlockItem VariableResolvingPhase)
  resolveLabelDeclaration (BlockStatement   stmt) = BlockStatement   <$> resolveLabelDeclaration stmt
  resolveLabelDeclaration (BlockDeclaration decl) = BlockDeclaration <$> resolveLabelDeclaration decl

  resolveLabelReference :: BlockItem VariableResolvingPhase -> SemanticAnalyzerMonad (BlockItem LabelResolvingPhase)
  resolveLabelReference (BlockStatement   stmt) = BlockStatement   <$> resolveLabelReference stmt
  resolveLabelReference (BlockDeclaration decl) = BlockDeclaration <$> resolveLabelReference decl

instance LabelResolver VarDeclaration where
  resolveLabelDeclaration :: VarDeclaration VariableResolvingPhase -> SemanticAnalyzerMonad (VarDeclaration VariableResolvingPhase)
  resolveLabelDeclaration (VarDeclaration () var initialisation) = VarDeclaration () var <$> (resolveLabelDeclaration <$?> initialisation)

  resolveLabelReference :: VarDeclaration VariableResolvingPhase -> SemanticAnalyzerMonad (VarDeclaration LabelResolvingPhase)
  resolveLabelReference (VarDeclaration () var initialisation) = VarDeclaration () var <$> (resolveLabelReference <$?> initialisation)

instance LabelResolver Statement where
  resolveLabelDeclaration :: Statement VariableResolvingPhase -> SemanticAnalyzerMonad (Statement VariableResolvingPhase)
  resolveLabelDeclaration (LabeledStatement label stmt) = LabeledStatement   <$> resolveLabelDeclaration label <*> resolveLabelDeclaration stmt
  resolveLabelDeclaration (UnlabeledStatement stmt)     = UnlabeledStatement <$> resolveLabelDeclaration stmt

  resolveLabelReference :: Statement VariableResolvingPhase -> SemanticAnalyzerMonad (Statement LabelResolvingPhase)
  resolveLabelReference (LabeledStatement label stmt) = LabeledStatement   <$> resolveLabelReference label <*> resolveLabelReference stmt
  resolveLabelReference (UnlabeledStatement stmt)     = UnlabeledStatement <$> resolveLabelReference stmt

instance LabelResolver UnlabeledStatement where
  resolveLabelDeclaration :: UnlabeledStatement VariableResolvingPhase -> SemanticAnalyzerMonad (UnlabeledStatement VariableResolvingPhase)
  resolveLabelDeclaration (Expression () expr)               = Expression () <$> resolveLabelDeclaration expr
  resolveLabelDeclaration (Compound   () block)              = Compound   () <$> resolveLabelDeclaration block
  resolveLabelDeclaration (If         () cond conseq altern) = If         () <$> resolveLabelDeclaration cond <*> resolveLabelDeclaration conseq <*> (resolveLabelDeclaration <$?> altern)
  resolveLabelDeclaration (Switch     () expr body)          = Switch     () <$> resolveLabelDeclaration expr <*> resolveLabelDeclaration body
  resolveLabelDeclaration (While      () cond body)          = While      () <$> resolveLabelDeclaration cond <*> resolveLabelDeclaration body
  resolveLabelDeclaration (DoWhile    () body cond)          = DoWhile    () <$> resolveLabelDeclaration body <*> resolveLabelDeclaration cond
  resolveLabelDeclaration (For        () ini cond post body) = For        () <$> resolveLabelDeclaration ini  <*> (resolveLabelDeclaration <$?> cond) <*> (resolveLabelDeclaration <$?> post) <*> resolveLabelDeclaration body
  resolveLabelDeclaration (Goto       () label)              = pure $ Goto     () label
  resolveLabelDeclaration (Continue   ())                    = pure $ Continue ()
  resolveLabelDeclaration (Break      ())                    = pure $ Break    ()
  resolveLabelDeclaration (Return     () expr)               = Return     () <$> resolveLabelDeclaration expr
  resolveLabelDeclaration (Null       ())                    = pure $ Null     ()

  resolveLabelReference :: UnlabeledStatement VariableResolvingPhase -> SemanticAnalyzerMonad (UnlabeledStatement LabelResolvingPhase)
  resolveLabelReference (Goto       () label)              = Goto       () <$> getLabel label
  resolveLabelReference (Return     () expr)               = Return     () <$> resolveLabelReference expr
  resolveLabelReference (Expression () expr)               = Expression () <$> resolveLabelReference expr
  resolveLabelReference (Compound   () block)              = Compound   () <$> resolveLabelReference block
  resolveLabelReference (If         () cond conseq altern) = If         () <$> resolveLabelReference cond <*> resolveLabelReference conseq <*> (resolveLabelReference <$?> altern)
  resolveLabelReference (Continue   ())                    = Continue <$> getEnclosingLoopLabel
  resolveLabelReference (Break      ())                    = Break    <$> getEnclosingLabel
  resolveLabelReference (Null       ())                    = pure $ Null     ()
  resolveLabelReference (Switch     () expr body)          =
    do switchLabel <- newLabel "switch"
       withEnclosingSwitch switchLabel $ Switch switchLabel <$> resolveLabelReference expr <*> resolveLabelReference body
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
  resolveLabelDeclaration :: ForInit VariableResolvingPhase -> SemanticAnalyzerMonad (ForInit VariableResolvingPhase)
  resolveLabelDeclaration (InitDecl decl) = InitDecl <$> resolveLabelDeclaration decl
  resolveLabelDeclaration (InitExp  expr) = InitExp  <$> (resolveLabelDeclaration <$?> expr)

  resolveLabelReference :: ForInit VariableResolvingPhase -> SemanticAnalyzerMonad (ForInit LabelResolvingPhase)
  resolveLabelReference (InitDecl decl) = InitDecl <$> resolveLabelReference decl
  resolveLabelReference (InitExp  expr) = InitExp  <$> (resolveLabelReference <$?> expr)

instance LabelResolver Label where
  resolveLabelDeclaration :: Label VariableResolvingPhase -> SemanticAnalyzerMonad (Label VariableResolvingPhase)
  resolveLabelDeclaration (Label   () label) = Label () <$> resolveLabel label
  resolveLabelDeclaration (Case    () expr)  = pure $ Case    () expr
  resolveLabelDeclaration (Default ())       = pure $ Default ()

  resolveLabelReference :: Label VariableResolvingPhase -> SemanticAnalyzerMonad (Label LabelResolvingPhase)
  resolveLabelReference (Case    () expr)  = Case    <$> newLabel "case" <*> resolveLabelReference expr
  resolveLabelReference (Default ())       = Default <$> newLabel "default"
  resolveLabelReference (Label   () label) = pure $ Label   () label

instance LabelResolver Exp where
  resolveLabelDeclaration :: Exp VariableResolvingPhase -> SemanticAnalyzerMonad (Exp VariableResolvingPhase)
  resolveLabelDeclaration (Constant    () cons)               = pure $ Constant () cons
  resolveLabelDeclaration (Var         () var)                = pure $ Var      () var
  resolveLabelDeclaration (Unary       () op expr)            = Unary       () op <$> resolveLabelDeclaration expr
  resolveLabelDeclaration (Binary      () op expr1 expr2)     = Binary      () op <$> resolveLabelDeclaration expr1 <*> resolveLabelDeclaration expr2
  resolveLabelDeclaration (Assignment  () lhs op rhs)         = Assignment  ()    <$> resolveLabelDeclaration lhs   <*> pure op                        <*> resolveLabelDeclaration rhs
  resolveLabelDeclaration (Conditional () cond conseq altern) = Conditional ()    <$> resolveLabelDeclaration cond  <*> resolveLabelDeclaration conseq <*> resolveLabelDeclaration altern

  resolveLabelReference :: Exp VariableResolvingPhase -> SemanticAnalyzerMonad (Exp LabelResolvingPhase)
  resolveLabelReference (Constant    () cons)               = pure $ Constant () cons
  resolveLabelReference (Var         () var)                = pure $ Var      () var
  resolveLabelReference (Unary       () op expr)            = Unary       () op <$> resolveLabelReference expr
  resolveLabelReference (Binary      () op expr1 expr2)     = Binary      () op <$> resolveLabelReference expr1 <*> resolveLabelReference expr2
  resolveLabelReference (Assignment  () lhs op rhs)         = Assignment  ()    <$> resolveLabelReference lhs   <*> pure op                        <*> resolveLabelReference rhs
  resolveLabelReference (Conditional () cond conseq altern) = Conditional ()    <$> resolveLabelReference cond  <*> resolveLabelReference conseq <*> resolveLabelReference altern


instance SwitchResolver Program where
  resolveSwitch :: Program LabelResolvingPhase -> SemanticAnalyzerMonad (Program SwitchResolvingPhase)
  resolveSwitch (Program func) = Program <$> mapM resolveSwitch func

instance SwitchResolver Declaration where
  resolveSwitch :: Declaration LabelResolvingPhase -> SemanticAnalyzerMonad (Declaration SwitchResolvingPhase)
  resolveSwitch (FunDecl decl) = FunDecl <$> resolveSwitch decl
  resolveSwitch (VarDecl decl) = VarDecl <$> resolveSwitch decl

instance SwitchResolver FunctionDeclaration where
  resolveSwitch :: FunctionDeclaration LabelResolvingPhase -> SemanticAnalyzerMonad (FunctionDeclaration SwitchResolvingPhase)
  resolveSwitch (FunctionDeclaration _ name params body) = modify (\env -> env { envFunctionName = Just name }) >> FunctionDeclaration () name params <$> (resolveSwitch <$?> body)

instance SwitchResolver Block where
  resolveSwitch :: Block LabelResolvingPhase -> SemanticAnalyzerMonad (Block SwitchResolvingPhase)
  resolveSwitch (Block blockItens) = Block <$> mapM resolveSwitch blockItens

instance SwitchResolver BlockItem where
  resolveSwitch :: BlockItem LabelResolvingPhase -> SemanticAnalyzerMonad (BlockItem SwitchResolvingPhase)
  resolveSwitch (BlockStatement   stmt) = BlockStatement   <$> resolveSwitch stmt
  resolveSwitch (BlockDeclaration decl) = BlockDeclaration <$> resolveSwitch decl

instance SwitchResolver VarDeclaration where
  resolveSwitch :: VarDeclaration LabelResolvingPhase -> SemanticAnalyzerMonad (VarDeclaration SwitchResolvingPhase)
  resolveSwitch (VarDeclaration () var initialisation) = VarDeclaration () var <$> (resolveSwitch <$?> initialisation)

instance SwitchResolver Statement where
  resolveSwitch :: Statement LabelResolvingPhase -> SemanticAnalyzerMonad (Statement SwitchResolvingPhase)
  resolveSwitch (LabeledStatement label stmt) = LabeledStatement   <$> resolveSwitch label <*> resolveSwitch stmt
  resolveSwitch (UnlabeledStatement stmt)     = UnlabeledStatement <$> resolveSwitch stmt

instance SwitchResolver UnlabeledStatement where
  resolveSwitch :: UnlabeledStatement LabelResolvingPhase -> SemanticAnalyzerMonad (UnlabeledStatement SwitchResolvingPhase)
  resolveSwitch (Expression () expr)               = Expression () <$> resolveSwitch expr
  resolveSwitch (Compound   () block)              = Compound   () <$> resolveSwitch block
  resolveSwitch (If         () cond conseq altern) = If         () <$> resolveSwitch cond <*> resolveSwitch conseq <*> (resolveSwitch <$?> altern)
  resolveSwitch (While      lb cond body)          = While      lb <$> resolveSwitch cond <*> resolveSwitch body
  resolveSwitch (DoWhile    lb body cond)          = DoWhile    lb <$> resolveSwitch body <*> resolveSwitch cond
  resolveSwitch (For        lb ini cond post body) = For        lb <$> resolveSwitch ini  <*> (resolveSwitch <$?> cond) <*> (resolveSwitch <$?> post) <*> resolveSwitch body
  resolveSwitch (Return     () expr)               = Return     () <$> resolveSwitch expr
  resolveSwitch (Goto       () label)              = pure $ Goto     () label
  resolveSwitch (Continue   lb)                    = pure $ Continue lb
  resolveSwitch (Break      lb)                    = pure $ Break    lb
  resolveSwitch (Null       ())                    = pure $ Null     ()
  resolveSwitch (Switch     label expr body)       =
    withSwitchLabels $
      do expr' <- resolveSwitch expr
         body' <- resolveSwitch body
         switchLabels <- gets envSwitchLabels
         case switchLabels of
           Just switchLabels' -> return $ Switch (label, switchLabels') expr' body'
           Nothing            -> throwError StatementOutsideSwitch

instance SwitchResolver ForInit where
  resolveSwitch :: ForInit LabelResolvingPhase -> SemanticAnalyzerMonad (ForInit SwitchResolvingPhase)
  resolveSwitch (InitDecl decl) = InitDecl <$> resolveSwitch decl
  resolveSwitch (InitExp  expr) = InitExp  <$> (resolveSwitch <$?> expr)

instance SwitchResolver Label where
  resolveSwitch :: Label LabelResolvingPhase -> SemanticAnalyzerMonad (Label SwitchResolvingPhase)
  resolveSwitch (Label () label)  = pure $ Label () label
  resolveSwitch (Case label expr) =
    do switchLabels <- gets envSwitchLabels
       constExpr <- collapseConstantExp expr
       case switchLabels of
         Nothing -> throwError StatementOutsideSwitch
         Just switchLabels' -> let key = SCase constExpr in
           if key `M.member` switchLabels' then
             throwError DuplicateCaseValue
           else
             modify (\env -> env { envSwitchLabels = Just $ M.insert (SCase constExpr) label switchLabels' })
       return $ Case label constExpr
  resolveSwitch (Default label) =
    do switchLabels <- gets envSwitchLabels
       case switchLabels of
         Nothing -> throwError StatementOutsideSwitch
         Just switchLabels' ->
           if SDefault `M.member` switchLabels' then
             throwError DuplicateDefaultValue
           else
             modify (\env -> env { envSwitchLabels = Just $ M.insert SDefault label switchLabels' })
       return $ Default label

instance SwitchResolver Exp where
  resolveSwitch :: Exp LabelResolvingPhase -> SemanticAnalyzerMonad (Exp SwitchResolvingPhase)
  resolveSwitch (Constant    () cons)               = pure $ Constant () cons
  resolveSwitch (Var         () var)                = pure $ Var      () var
  resolveSwitch (Unary       () op expr)            = Unary       () op <$> resolveSwitch expr
  resolveSwitch (Binary      () op expr1 expr2)     = Binary      () op <$> resolveSwitch expr1 <*> resolveSwitch expr2
  resolveSwitch (Assignment  () lhs op rhs)         = Assignment  ()    <$> resolveSwitch lhs   <*> pure op              <*> resolveSwitch rhs
  resolveSwitch (Conditional () cond conseq altern) = Conditional ()    <$> resolveSwitch cond  <*> resolveSwitch conseq <*> resolveSwitch altern
