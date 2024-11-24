{-# LANGUAGE DataKinds         #-}
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
                                         XFunctionCall, XGoto, XIf, XLabel,
                                         XNull, XReturn, XSwitch, XUnary, XVar,
                                         XVarDecl, XWhile)
import           SemanticAnalyzer.Error (SemanticError (..))

import           Control.Applicative    (asum)
import           Control.Monad          (when)
import           Control.Monad.Except   (Except, MonadError (..))
import           Control.Monad.State    (StateT, gets, modify)
import           Data.Bits              (Bits (complement), shiftL, shiftR, xor,
                                         (.&.), (.|.))
import           Data.Map               (Map, (!))
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe, isJust, isNothing)
import           Data.Set               (Set)
import qualified Data.Set               as S
import qualified Data.Text              as T
import           Numeric.Natural        (Natural)
import           Parser.ParserMonad     (ParserPhase)

data IdentifierResolutionPhase
data LabelResolvingPhase
data SwitchResolvingPhase
data TypeCheckingPhase

type instance XExpression   IdentifierResolutionPhase = ()
type instance XCompound     IdentifierResolutionPhase = ()
type instance XIf           IdentifierResolutionPhase = ()
type instance XSwitch       IdentifierResolutionPhase = ()
type instance XWhile        IdentifierResolutionPhase = ()
type instance XDoWhile      IdentifierResolutionPhase = ()
type instance XFor          IdentifierResolutionPhase = ()
type instance XGoto         IdentifierResolutionPhase = ()
type instance XContinue     IdentifierResolutionPhase = ()
type instance XBreak        IdentifierResolutionPhase = ()
type instance XReturn       IdentifierResolutionPhase = ()
type instance XNull         IdentifierResolutionPhase = ()
type instance XLabel        IdentifierResolutionPhase = ()
type instance XDefault      IdentifierResolutionPhase = ()
type instance XCase         IdentifierResolutionPhase = ()
type instance XCaseV        IdentifierResolutionPhase = Exp IdentifierResolutionPhase
type instance XConstant     IdentifierResolutionPhase = ()
type instance XVar          IdentifierResolutionPhase = ()
type instance XUnary        IdentifierResolutionPhase = ()
type instance XBinary       IdentifierResolutionPhase = ()
type instance XAssignment   IdentifierResolutionPhase = ()
type instance XConditional  IdentifierResolutionPhase = ()
type instance XFunDecl      IdentifierResolutionPhase = ()
type instance XVarDecl      IdentifierResolutionPhase = ()
type instance XFunctionCall IdentifierResolutionPhase = ()

type instance XExpression   LabelResolvingPhase = ()
type instance XCompound     LabelResolvingPhase = ()
type instance XIf           LabelResolvingPhase = ()
type instance XSwitch       LabelResolvingPhase = Identifier
type instance XWhile        LabelResolvingPhase = Identifier
type instance XDoWhile      LabelResolvingPhase = Identifier
type instance XFor          LabelResolvingPhase = Identifier
type instance XGoto         LabelResolvingPhase = ()
type instance XContinue     LabelResolvingPhase = Identifier
type instance XBreak        LabelResolvingPhase = Identifier
type instance XReturn       LabelResolvingPhase = ()
type instance XNull         LabelResolvingPhase = ()
type instance XLabel        LabelResolvingPhase = ()
type instance XDefault      LabelResolvingPhase = Identifier
type instance XCase         LabelResolvingPhase = Identifier
type instance XCaseV        LabelResolvingPhase = Exp LabelResolvingPhase
type instance XConstant     LabelResolvingPhase = ()
type instance XVar          LabelResolvingPhase = ()
type instance XUnary        LabelResolvingPhase = ()
type instance XBinary       LabelResolvingPhase = ()
type instance XAssignment   LabelResolvingPhase = ()
type instance XConditional  LabelResolvingPhase = ()
type instance XFunDecl      LabelResolvingPhase = ()
type instance XVarDecl      LabelResolvingPhase = ()
type instance XFunctionCall LabelResolvingPhase = ()

data SwitchLabel = SCase Constant
                 | SDefault
  deriving (Show, Eq, Ord)

type instance XExpression   SwitchResolvingPhase = ()
type instance XCompound     SwitchResolvingPhase = ()
type instance XIf           SwitchResolvingPhase = ()
type instance XSwitch       SwitchResolvingPhase = (Identifier, Map SwitchLabel Identifier)
type instance XWhile        SwitchResolvingPhase = Identifier
type instance XDoWhile      SwitchResolvingPhase = Identifier
type instance XFor          SwitchResolvingPhase = Identifier
type instance XGoto         SwitchResolvingPhase = ()
type instance XContinue     SwitchResolvingPhase = Identifier
type instance XBreak        SwitchResolvingPhase = Identifier
type instance XReturn       SwitchResolvingPhase = ()
type instance XNull         SwitchResolvingPhase = ()
type instance XLabel        SwitchResolvingPhase = ()
type instance XDefault      SwitchResolvingPhase = Identifier
type instance XCase         SwitchResolvingPhase = Identifier
type instance XCaseV        SwitchResolvingPhase = Constant
type instance XConstant     SwitchResolvingPhase = ()
type instance XVar          SwitchResolvingPhase = ()
type instance XUnary        SwitchResolvingPhase = ()
type instance XBinary       SwitchResolvingPhase = ()
type instance XAssignment   SwitchResolvingPhase = ()
type instance XConditional  SwitchResolvingPhase = ()
type instance XFunDecl      SwitchResolvingPhase = ()
type instance XVarDecl      SwitchResolvingPhase = ()
type instance XFunctionCall SwitchResolvingPhase = ()

data Typ = Int
         | Fun [Typ] Typ
  deriving (Show, Eq, Ord)

type instance XExpression   TypeCheckingPhase = Typ
type instance XCompound     TypeCheckingPhase = ()
type instance XIf           TypeCheckingPhase = ()
type instance XSwitch       TypeCheckingPhase = (Identifier, Map SwitchLabel Identifier)
type instance XWhile        TypeCheckingPhase = Identifier
type instance XDoWhile      TypeCheckingPhase = Identifier
type instance XFor          TypeCheckingPhase = Identifier
type instance XGoto         TypeCheckingPhase = ()
type instance XContinue     TypeCheckingPhase = Identifier
type instance XBreak        TypeCheckingPhase = Identifier
type instance XReturn       TypeCheckingPhase = ()
type instance XNull         TypeCheckingPhase = ()
type instance XLabel        TypeCheckingPhase = ()
type instance XDefault      TypeCheckingPhase = Identifier
type instance XCase         TypeCheckingPhase = Identifier
type instance XCaseV        TypeCheckingPhase = Constant
type instance XConstant     TypeCheckingPhase = Typ
type instance XVar          TypeCheckingPhase = Typ
type instance XUnary        TypeCheckingPhase = Typ
type instance XBinary       TypeCheckingPhase = Typ
type instance XAssignment   TypeCheckingPhase = Typ
type instance XConditional  TypeCheckingPhase = Typ
type instance XFunctionCall TypeCheckingPhase = Typ
type instance XFunDecl      TypeCheckingPhase = Typ
type instance XVarDecl      TypeCheckingPhase = Typ

data EnclosingLabel = LLoop   Identifier
                    | LSwitch Identifier

data Environment = Environment { envFunctionName       :: Maybe Identifier
                               , envIdentifierCounter  :: Natural
                               , envCurrentScope       :: Map Identifier (Identifier, Bool)
                               , envEnclosingScopes    :: [Map Identifier (Identifier, Bool)]
                               , envLabelEnv           :: Map Identifier Identifier
                               , envEnclosingLabels    :: [EnclosingLabel]
                               , envSwitchLabels       :: Maybe (Map SwitchLabel Identifier)
                               , envSymbolTypes        :: Map Identifier Typ
                               , envDefinedFunctions   :: Set Identifier
                               }

emptyEnvironment :: Environment
emptyEnvironment = Environment { envFunctionName       = Nothing
                               , envIdentifierCounter  = 0
                               , envCurrentScope       = M.empty
                               , envEnclosingScopes    = []
                               , envLabelEnv           = M.empty
                               , envEnclosingLabels    = []
                               , envSwitchLabels       = Nothing
                               , envSymbolTypes        = M.empty
                               , envDefinedFunctions   = S.empty
                               }

type SemanticAnalyzerMonad a = StateT Environment (Except SemanticError) a

resolveNewIdentifier :: Bool -> Identifier -> SemanticAnalyzerMonad Identifier
resolveNewIdentifier hasLinkage name =
  do currentScope <- gets envCurrentScope
     case M.lookup name currentScope of
       Just (_, False)                 -> throwError $ DuplicateDeclaration name
       Just (_, True) | not hasLinkage -> throwError $ DuplicateDeclaration name
       _                               ->
          do name' <- if hasLinkage then
                         return name
                        else do
                          identifierCounter <- gets envIdentifierCounter
                          let identifierCounter' = succ identifierCounter
                          modify (\env -> env { envIdentifierCounter = identifierCounter' })
                          return $ name <> "." <> T.pack (show identifierCounter')
             let currentScope' = M.insert name (name', hasLinkage) currentScope
             modify (\env -> env { envCurrentScope = currentScope' })
             return name'

getIdentifier :: Identifier -> SemanticAnalyzerMonad Identifier
getIdentifier identifier = do currentScopeVarEnv <- gets envCurrentScope
                              upperScopeVarEnvs  <- gets envEnclosingScopes
                              case asum $ map (M.lookup identifier) $ currentScopeVarEnv : upperScopeVarEnvs of
                                Nothing          -> throwError $ UndefinedIdentifierUse identifier
                                Just (identifier', _) -> return identifier'

resolveLabel :: Identifier -> SemanticAnalyzerMonad Identifier
resolveLabel label =
  do functionName <- gets envFunctionName
     labelCounter <- gets envIdentifierCounter
     labelEnv     <- gets envLabelEnv
     let label' = maybe "" (<> ".") functionName <> label
     if label' `M.member` labelEnv then
       throwError $ DuplicateLabelDeclaration label
     else do
       let labelCounter' = succ labelCounter
       let label'' = label' <> "." <> T.pack (show labelCounter')
       let labelEnv' = M.insert label' label'' labelEnv
       modify (\env -> env { envLabelEnv = labelEnv', envIdentifierCounter = labelCounter' })
       return label''

newLabel :: Identifier -> SemanticAnalyzerMonad Identifier
newLabel caption =
  do functionName <- gets envFunctionName
     labelCounter <- gets envIdentifierCounter
     let labelCounter' = succ labelCounter
     let label' = fromMaybe "" functionName <> "." <> caption <> "." <> T.pack (show labelCounter')
     modify (\env -> env { envIdentifierCounter = labelCounter' })
     return label'

getLabel :: Identifier -> SemanticAnalyzerMonad Identifier
getLabel label = do functionName <- gets envFunctionName
                    labelEnv <- gets envLabelEnv
                    let label' = maybe "" (<> ".") functionName <> label
                    case M.lookup label' labelEnv of
                      Nothing      -> throwError $ UndefinedLabelUse label
                      Just label'' -> return label''

localState :: (Environment -> Environment) -> (Environment -> Environment) -> SemanticAnalyzerMonad a -> SemanticAnalyzerMonad a
localState push pop action =
  do modify push
     result <- action
     modify pop
     return result

withNewVarScope :: SemanticAnalyzerMonad a -> SemanticAnalyzerMonad a
withNewVarScope action =
    do currentScopeVarEnv <- gets envCurrentScope
       upperScopesVarEnvs <- gets envEnclosingScopes
       localState
         (\env -> env { envCurrentScope = M.empty, envEnclosingScopes = currentScopeVarEnv : upperScopesVarEnvs })
         (\env -> env { envCurrentScope = currentScopeVarEnv, envEnclosingScopes = upperScopesVarEnvs })
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

getExpType :: Exp TypeCheckingPhase -> Typ
getExpType (Constant     typ _)     = typ
getExpType (Var          typ _)     = typ
getExpType (Unary        typ _ _)   = typ
getExpType (Binary       typ _ _ _) = typ
getExpType (Assignment   typ _ _ _) = typ
getExpType (Conditional  typ _ _ _) = typ
getExpType (FunctionCall typ _ _)   = typ

getConstType :: Constant-> Typ
getConstType (CInt _) = Int

addSymbolType :: Identifier -> Typ -> SemanticAnalyzerMonad ()
addSymbolType name typ =
  do symbolTypes <- gets envSymbolTypes
     modify (\env -> env { envSymbolTypes = M.insert name typ symbolTypes })


(<$?>) :: Applicative f => (t -> f a) -> Maybe t -> f (Maybe a)
f <$?> x = case x of
  Just x' -> Just <$> f x'
  Nothing -> pure Nothing

class IdentifierResolver a where
  resolveIdentifiers :: a ParserPhase -> SemanticAnalyzerMonad (a IdentifierResolutionPhase)

class LabelResolver a where
  resolveLabelDeclaration :: a IdentifierResolutionPhase -> SemanticAnalyzerMonad (a IdentifierResolutionPhase)
  resolveLabelReference :: a IdentifierResolutionPhase -> SemanticAnalyzerMonad (a LabelResolvingPhase)

class SwitchResolver a where
  resolveSwitch :: a LabelResolvingPhase -> SemanticAnalyzerMonad (a SwitchResolvingPhase)

class TypeChecker a where
  resolveTypes :: a SwitchResolvingPhase -> SemanticAnalyzerMonad (a TypeCheckingPhase)


instance IdentifierResolver Program where
  resolveIdentifiers :: Program ParserPhase -> SemanticAnalyzerMonad (Program IdentifierResolutionPhase)
  resolveIdentifiers (Program func) = Program <$> mapM resolveIdentifiers func

instance IdentifierResolver Declaration where
  resolveIdentifiers :: Declaration ParserPhase -> SemanticAnalyzerMonad (Declaration IdentifierResolutionPhase)
  resolveIdentifiers (FunDecl decl) = FunDecl <$> resolveIdentifiers decl
  resolveIdentifiers (VarDecl decl) = VarDecl <$> resolveIdentifiers decl

instance IdentifierResolver FunctionDeclaration where
  resolveIdentifiers :: FunctionDeclaration ParserPhase -> SemanticAnalyzerMonad (FunctionDeclaration IdentifierResolutionPhase)
  resolveIdentifiers (FunctionDeclaration _ name params body) =
    do name' <- resolveNewIdentifier True name
       currentName <- gets envFunctionName
       modify (\env -> env { envFunctionName = Just name })
       fun <- withNewVarScope $ do
         params' <- mapM (resolveNewIdentifier False) params
         body' <- case body of
                    Nothing            -> return Nothing
                    Just (Block body') -> if isNothing currentName then Just <$> (Block <$> mapM resolveIdentifiers body')
                                                                   else throwError NestedFunctionDefinition
         return $ FunctionDeclaration () name' params' body'
       modify (\env -> env { envFunctionName = currentName })
       return fun

instance IdentifierResolver Block where
  resolveIdentifiers :: Block ParserPhase -> SemanticAnalyzerMonad (Block IdentifierResolutionPhase)
  resolveIdentifiers (Block blockItens) = withNewVarScope $ Block <$> mapM resolveIdentifiers blockItens

instance IdentifierResolver BlockItem where
  resolveIdentifiers :: BlockItem ParserPhase -> SemanticAnalyzerMonad (BlockItem IdentifierResolutionPhase)
  resolveIdentifiers (BlockStatement   stmt) = BlockStatement   <$> resolveIdentifiers stmt
  resolveIdentifiers (BlockDeclaration decl) = BlockDeclaration <$> resolveIdentifiers decl

instance IdentifierResolver VarDeclaration where
  resolveIdentifiers :: VarDeclaration ParserPhase -> SemanticAnalyzerMonad (VarDeclaration IdentifierResolutionPhase)
  resolveIdentifiers (VarDeclaration () var initialisation) = VarDeclaration () <$> resolveNewIdentifier False var <*> (resolveIdentifiers <$?> initialisation)

instance IdentifierResolver Statement where
  resolveIdentifiers :: Statement ParserPhase -> SemanticAnalyzerMonad (Statement IdentifierResolutionPhase)
  resolveIdentifiers (LabeledStatement label stmt) = LabeledStatement   <$> resolveIdentifiers label <*> resolveIdentifiers stmt
  resolveIdentifiers (UnlabeledStatement stmt)     = UnlabeledStatement <$> resolveIdentifiers stmt

instance IdentifierResolver UnlabeledStatement where
  resolveIdentifiers :: UnlabeledStatement ParserPhase -> SemanticAnalyzerMonad (UnlabeledStatement IdentifierResolutionPhase)
  resolveIdentifiers (Expression () expr)               = Expression () <$> resolveIdentifiers expr
  resolveIdentifiers (Compound   () block)              = Compound   () <$> resolveIdentifiers block
  resolveIdentifiers (If         () cond conseq altern) = If         () <$> resolveIdentifiers cond <*> resolveIdentifiers conseq <*> (resolveIdentifiers <$?> altern)
  resolveIdentifiers (Switch     () expr body)          = Switch     () <$> resolveIdentifiers expr <*> resolveIdentifiers body
  resolveIdentifiers (While      () cond body)          = While      () <$> resolveIdentifiers cond <*> resolveIdentifiers body
  resolveIdentifiers (DoWhile    () body cond)          = DoWhile    () <$> resolveIdentifiers body <*> resolveIdentifiers cond
  resolveIdentifiers (For        () ini cond post body) = withNewVarScope $ For () <$> resolveIdentifiers ini <*> (resolveIdentifiers <$?> cond) <*> (resolveIdentifiers <$?> post) <*> resolveIdentifiers body
  resolveIdentifiers (Goto       () label)              = pure $ Goto     () label
  resolveIdentifiers (Continue   ())                    = pure $ Continue ()
  resolveIdentifiers (Break      ())                    = pure $ Break    ()
  resolveIdentifiers (Return     () expr)               = Return     () <$> resolveIdentifiers expr
  resolveIdentifiers (Null       ())                    = pure $ Null     ()

instance IdentifierResolver ForInit where
  resolveIdentifiers :: ForInit ParserPhase -> SemanticAnalyzerMonad (ForInit IdentifierResolutionPhase)
  resolveIdentifiers (InitDecl  decl) = InitDecl <$> resolveIdentifiers decl
  resolveIdentifiers (InitExp   expr) = InitExp  <$> (resolveIdentifiers <$?> expr)

instance IdentifierResolver Label where
  resolveIdentifiers :: Label ParserPhase -> SemanticAnalyzerMonad (Label IdentifierResolutionPhase)
  resolveIdentifiers (Case    () expr)  = Case () <$> resolveIdentifiers expr
  resolveIdentifiers (Label   () label) = pure $ Label   () label
  resolveIdentifiers (Default ())       = pure $ Default ()

instance IdentifierResolver Exp where
  resolveIdentifiers :: Exp ParserPhase -> SemanticAnalyzerMonad (Exp IdentifierResolutionPhase)
  resolveIdentifiers (Constant     () cons)                                         = pure $ Constant () cons
  resolveIdentifiers (Var          () var)                                          = Var          ()    <$> getIdentifier var
  resolveIdentifiers (Unary        () op@(UnaryAssignmentOperator _) lhs@(Var _ _)) = Unary        () op <$> resolveIdentifiers lhs
  resolveIdentifiers (Unary        () (UnaryAssignmentOperator _)    _)             = throwError InvalidLHS
  resolveIdentifiers (Unary        () op expr)                                      = Unary        () op <$> resolveIdentifiers expr
  resolveIdentifiers (Binary       () op expr1 expr2)                               = Binary       () op <$> resolveIdentifiers expr1 <*> resolveIdentifiers expr2
  resolveIdentifiers (Assignment   () lhs@(Var _ _) op rhs)                         = Assignment   ()    <$> resolveIdentifiers lhs   <*> pure op                <*> resolveIdentifiers rhs
  resolveIdentifiers (Assignment   () _ _ _)                                        = throwError InvalidLHS
  resolveIdentifiers (Conditional  () cond conseq altern)                           = Conditional  ()    <$> resolveIdentifiers cond  <*> resolveIdentifiers conseq <*> resolveIdentifiers altern
  resolveIdentifiers (FunctionCall () name args)                                    = FunctionCall ()    <$> getIdentifier name       <*> mapM resolveIdentifiers args


instance LabelResolver Program where
  resolveLabelDeclaration :: Program IdentifierResolutionPhase -> SemanticAnalyzerMonad (Program IdentifierResolutionPhase)
  resolveLabelDeclaration (Program func) = Program <$> mapM resolveLabelDeclaration func

  resolveLabelReference :: Program IdentifierResolutionPhase -> SemanticAnalyzerMonad (Program LabelResolvingPhase)
  resolveLabelReference (Program func) = Program <$> mapM resolveLabelReference func

instance LabelResolver Declaration where
  resolveLabelDeclaration :: Declaration IdentifierResolutionPhase -> SemanticAnalyzerMonad (Declaration IdentifierResolutionPhase)
  resolveLabelDeclaration (FunDecl decl) = FunDecl <$> resolveLabelDeclaration decl
  resolveLabelDeclaration (VarDecl decl) = VarDecl <$> resolveLabelDeclaration decl

  resolveLabelReference :: Declaration IdentifierResolutionPhase -> SemanticAnalyzerMonad (Declaration LabelResolvingPhase)
  resolveLabelReference (FunDecl decl) = FunDecl <$> resolveLabelReference decl
  resolveLabelReference (VarDecl decl) = VarDecl <$> resolveLabelReference decl

instance LabelResolver FunctionDeclaration where
  resolveLabelDeclaration :: FunctionDeclaration IdentifierResolutionPhase -> SemanticAnalyzerMonad (FunctionDeclaration IdentifierResolutionPhase)
  resolveLabelDeclaration (FunctionDeclaration _ name params body) = modify (\env -> env { envFunctionName = Just name }) >> FunctionDeclaration () name params <$> (resolveLabelDeclaration <$?> body)

  resolveLabelReference :: FunctionDeclaration IdentifierResolutionPhase -> SemanticAnalyzerMonad (FunctionDeclaration LabelResolvingPhase)
  resolveLabelReference (FunctionDeclaration _ name params body) = modify (\env -> env { envFunctionName = Just name }) >> FunctionDeclaration () name params <$> resolveLabelReference <$?> body

instance LabelResolver Block where
  resolveLabelDeclaration :: Block IdentifierResolutionPhase -> SemanticAnalyzerMonad (Block IdentifierResolutionPhase)
  resolveLabelDeclaration (Block blockItens) = Block <$> mapM resolveLabelDeclaration blockItens

  resolveLabelReference :: Block IdentifierResolutionPhase -> SemanticAnalyzerMonad (Block LabelResolvingPhase)
  resolveLabelReference (Block blockItens) = Block <$> mapM resolveLabelReference blockItens

instance LabelResolver BlockItem where
  resolveLabelDeclaration :: BlockItem IdentifierResolutionPhase -> SemanticAnalyzerMonad (BlockItem IdentifierResolutionPhase)
  resolveLabelDeclaration (BlockStatement   stmt) = BlockStatement   <$> resolveLabelDeclaration stmt
  resolveLabelDeclaration (BlockDeclaration decl) = BlockDeclaration <$> resolveLabelDeclaration decl

  resolveLabelReference :: BlockItem IdentifierResolutionPhase -> SemanticAnalyzerMonad (BlockItem LabelResolvingPhase)
  resolveLabelReference (BlockStatement   stmt) = BlockStatement   <$> resolveLabelReference stmt
  resolveLabelReference (BlockDeclaration decl) = BlockDeclaration <$> resolveLabelReference decl

instance LabelResolver VarDeclaration where
  resolveLabelDeclaration :: VarDeclaration IdentifierResolutionPhase -> SemanticAnalyzerMonad (VarDeclaration IdentifierResolutionPhase)
  resolveLabelDeclaration (VarDeclaration () var initialisation) = VarDeclaration () var <$> (resolveLabelDeclaration <$?> initialisation)

  resolveLabelReference :: VarDeclaration IdentifierResolutionPhase -> SemanticAnalyzerMonad (VarDeclaration LabelResolvingPhase)
  resolveLabelReference (VarDeclaration () var initialisation) = VarDeclaration () var <$> (resolveLabelReference <$?> initialisation)

instance LabelResolver Statement where
  resolveLabelDeclaration :: Statement IdentifierResolutionPhase -> SemanticAnalyzerMonad (Statement IdentifierResolutionPhase)
  resolveLabelDeclaration (LabeledStatement label stmt) = LabeledStatement   <$> resolveLabelDeclaration label <*> resolveLabelDeclaration stmt
  resolveLabelDeclaration (UnlabeledStatement stmt)     = UnlabeledStatement <$> resolveLabelDeclaration stmt

  resolveLabelReference :: Statement IdentifierResolutionPhase -> SemanticAnalyzerMonad (Statement LabelResolvingPhase)
  resolveLabelReference (LabeledStatement label stmt) = LabeledStatement   <$> resolveLabelReference label <*> resolveLabelReference stmt
  resolveLabelReference (UnlabeledStatement stmt)     = UnlabeledStatement <$> resolveLabelReference stmt

instance LabelResolver UnlabeledStatement where
  resolveLabelDeclaration :: UnlabeledStatement IdentifierResolutionPhase -> SemanticAnalyzerMonad (UnlabeledStatement IdentifierResolutionPhase)
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

  resolveLabelReference :: UnlabeledStatement IdentifierResolutionPhase -> SemanticAnalyzerMonad (UnlabeledStatement LabelResolvingPhase)
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
  resolveLabelDeclaration :: ForInit IdentifierResolutionPhase -> SemanticAnalyzerMonad (ForInit IdentifierResolutionPhase)
  resolveLabelDeclaration (InitDecl decl) = InitDecl <$> resolveLabelDeclaration decl
  resolveLabelDeclaration (InitExp  expr) = InitExp  <$> (resolveLabelDeclaration <$?> expr)

  resolveLabelReference :: ForInit IdentifierResolutionPhase -> SemanticAnalyzerMonad (ForInit LabelResolvingPhase)
  resolveLabelReference (InitDecl decl) = InitDecl <$> resolveLabelReference decl
  resolveLabelReference (InitExp  expr) = InitExp  <$> (resolveLabelReference <$?> expr)

instance LabelResolver Label where
  resolveLabelDeclaration :: Label IdentifierResolutionPhase -> SemanticAnalyzerMonad (Label IdentifierResolutionPhase)
  resolveLabelDeclaration (Label   () label) = Label () <$> resolveLabel label
  resolveLabelDeclaration (Case    () expr)  = pure $ Case    () expr
  resolveLabelDeclaration (Default ())       = pure $ Default ()

  resolveLabelReference :: Label IdentifierResolutionPhase -> SemanticAnalyzerMonad (Label LabelResolvingPhase)
  resolveLabelReference (Case    () expr)  = Case    <$> newLabel "case" <*> resolveLabelReference expr
  resolveLabelReference (Default ())       = Default <$> newLabel "default"
  resolveLabelReference (Label   () label) = pure $ Label   () label

instance LabelResolver Exp where
  resolveLabelDeclaration :: Exp IdentifierResolutionPhase -> SemanticAnalyzerMonad (Exp IdentifierResolutionPhase)
  resolveLabelDeclaration (Constant     () cons)               = pure $ Constant     () cons
  resolveLabelDeclaration (Var          () var)                = pure $ Var          () var
  resolveLabelDeclaration (Unary        () op expr)            = Unary        () op   <$> resolveLabelDeclaration expr
  resolveLabelDeclaration (Binary       () op expr1 expr2)     = Binary       () op   <$> resolveLabelDeclaration expr1     <*> resolveLabelDeclaration expr2
  resolveLabelDeclaration (Assignment   () lhs op rhs)         = Assignment   ()      <$> resolveLabelDeclaration lhs       <*> pure op                        <*> resolveLabelDeclaration rhs
  resolveLabelDeclaration (Conditional  () cond conseq altern) = Conditional  ()      <$> resolveLabelDeclaration cond      <*> resolveLabelDeclaration conseq <*> resolveLabelDeclaration altern
  resolveLabelDeclaration (FunctionCall () name args)          = FunctionCall () name <$> mapM resolveLabelDeclaration args

  resolveLabelReference :: Exp IdentifierResolutionPhase -> SemanticAnalyzerMonad (Exp LabelResolvingPhase)
  resolveLabelReference (Constant     () cons)               = pure $ Constant () cons
  resolveLabelReference (Var          () var)                = pure $ Var      () var
  resolveLabelReference (Unary        () op expr)            = Unary        () op  <$> resolveLabelReference expr
  resolveLabelReference (Binary       () op expr1 expr2)     = Binary       () op  <$> resolveLabelReference expr1 <*> resolveLabelReference expr2
  resolveLabelReference (Assignment   () lhs op rhs)         = Assignment   ()     <$> resolveLabelReference lhs   <*> pure op                        <*> resolveLabelReference rhs
  resolveLabelReference (Conditional  () cond conseq altern) = Conditional  ()     <$> resolveLabelReference cond  <*> resolveLabelReference conseq <*> resolveLabelReference altern
  resolveLabelReference (FunctionCall () name args)          = FunctionCall () name <$> mapM resolveLabelReference args


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
  resolveSwitch (Constant     () cons)               = pure $ Constant () cons
  resolveSwitch (Var          () var)                = pure $ Var      () var
  resolveSwitch (Unary        () op expr)            = Unary        () op   <$> resolveSwitch expr
  resolveSwitch (Binary       () op expr1 expr2)     = Binary       () op   <$> resolveSwitch expr1     <*> resolveSwitch expr2
  resolveSwitch (Assignment   () lhs op rhs)         = Assignment   ()      <$> resolveSwitch lhs       <*> pure op              <*> resolveSwitch rhs
  resolveSwitch (Conditional  () cond conseq altern) = Conditional  ()      <$> resolveSwitch cond      <*> resolveSwitch conseq <*> resolveSwitch altern
  resolveSwitch (FunctionCall () name args)          = FunctionCall () name <$> mapM resolveSwitch args


instance TypeChecker Program where
  resolveTypes :: Program SwitchResolvingPhase -> SemanticAnalyzerMonad (Program TypeCheckingPhase)
  resolveTypes (Program func) = Program <$> mapM resolveTypes func

instance TypeChecker Declaration where
  resolveTypes :: Declaration SwitchResolvingPhase -> SemanticAnalyzerMonad (Declaration TypeCheckingPhase)
  resolveTypes (FunDecl decl) = FunDecl <$> resolveTypes decl
  resolveTypes (VarDecl decl) = VarDecl <$> resolveTypes decl

instance TypeChecker VarDeclaration where
  resolveTypes :: VarDeclaration SwitchResolvingPhase -> SemanticAnalyzerMonad (VarDeclaration TypeCheckingPhase)
  resolveTypes (VarDeclaration () name initialisation) =
    do let varType = Int
       addSymbolType name varType
       initialisation' <- case initialisation of
         Nothing   -> return Nothing
         Just ini' -> do initialisation' <- resolveTypes ini'
                         when (varType /= getExpType initialisation') $ throwError IncompatibleTypes
                         return $ Just initialisation'

       return $ VarDeclaration varType name initialisation'

instance TypeChecker FunctionDeclaration where
  resolveTypes :: FunctionDeclaration SwitchResolvingPhase -> SemanticAnalyzerMonad (FunctionDeclaration TypeCheckingPhase)
  resolveTypes (FunctionDeclaration () name params body) = -- FunctionDeclaration (Fun (map (const Int) params) Int) name params <$> (resolveTypes <$?> body)
    do symbolTypes      <- gets envSymbolTypes
       definedFunctions <- gets envDefinedFunctions

       let funType = Fun (map (const Int) params) Int
       let hasBody = isJust body

       case M.lookup name symbolTypes of
         Just funType' -> do when (funType' /= funType) $ throwError IncompatibleTypes
                             when (hasBody && S.member name definedFunctions) $ throwError FunctionDefinedMoreThanOnce
         Nothing       -> pure ()

       modify (\env -> env { envSymbolTypes = M.insert name funType symbolTypes })

       body' <- case body of
         Just b -> do mapM_ (`addSymbolType` Int) params
                      modify (\env -> env { envDefinedFunctions = S.insert name definedFunctions })
                      Just <$> resolveTypes b
         Nothing -> return Nothing

       return $ FunctionDeclaration funType name params body'


instance TypeChecker Block where
  resolveTypes :: Block SwitchResolvingPhase -> SemanticAnalyzerMonad (Block TypeCheckingPhase)
  resolveTypes (Block blockItens) = Block <$> mapM resolveTypes blockItens

instance TypeChecker BlockItem where
  resolveTypes :: BlockItem SwitchResolvingPhase -> SemanticAnalyzerMonad (BlockItem TypeCheckingPhase)
  resolveTypes (BlockStatement   stmt) = BlockStatement   <$> resolveTypes stmt
  resolveTypes (BlockDeclaration decl) = BlockDeclaration <$> resolveTypes decl

instance TypeChecker Statement where
  resolveTypes :: Statement SwitchResolvingPhase -> SemanticAnalyzerMonad (Statement TypeCheckingPhase)
  resolveTypes (LabeledStatement label stmt) = LabeledStatement   <$> resolveTypes label <*> resolveTypes stmt
  resolveTypes (UnlabeledStatement stmt)     = UnlabeledStatement <$> resolveTypes stmt

instance TypeChecker UnlabeledStatement where
  resolveTypes :: UnlabeledStatement SwitchResolvingPhase -> SemanticAnalyzerMonad (UnlabeledStatement TypeCheckingPhase)
  resolveTypes (Expression () expr)               = resolveTypes expr >>= \expr' -> return $ Expression (getExpType expr') expr'
  resolveTypes (Compound   () block)              = Compound   () <$> resolveTypes block
  resolveTypes (If         () cond conseq altern) = If         () <$> resolveTypes cond  <*> resolveTypes conseq <*> (resolveTypes <$?> altern)
  resolveTypes (While      lb cond body)          = While      lb <$> resolveTypes cond  <*> resolveTypes body
  resolveTypes (DoWhile    lb body cond)          = DoWhile    lb <$> resolveTypes body  <*> resolveTypes cond
  resolveTypes (For        lb ini cond post body) = For        lb <$> resolveTypes ini   <*> (resolveTypes <$?> cond) <*> (resolveTypes <$?> post) <*> resolveTypes body
  resolveTypes (Switch     lb expr body)          = Switch     lb <$> resolveTypes expr  <*> resolveTypes body
  resolveTypes (Return     () expr)               = Return     () <$> resolveTypes expr
  resolveTypes (Goto       () label)              = pure $ Goto     () label
  resolveTypes (Continue   lb)                    = pure $ Continue lb
  resolveTypes (Break      lb)                    = pure $ Break    lb
  resolveTypes (Null       ())                    = pure $ Null     ()

instance TypeChecker ForInit where
  resolveTypes :: ForInit SwitchResolvingPhase -> SemanticAnalyzerMonad (ForInit TypeCheckingPhase)
  resolveTypes (InitDecl decl) = InitDecl <$> resolveTypes decl
  resolveTypes (InitExp  expr) = InitExp  <$> (resolveTypes <$?> expr)

instance TypeChecker Label where
  resolveTypes :: Label SwitchResolvingPhase -> SemanticAnalyzerMonad (Label TypeCheckingPhase)
  resolveTypes (Label () label)  = pure $ Label () label
  resolveTypes (Case label expr) = pure $ Case label expr
  resolveTypes (Default label)   = pure $ Default label

instance TypeChecker Exp where
  resolveTypes :: Exp SwitchResolvingPhase -> SemanticAnalyzerMonad (Exp TypeCheckingPhase)
  resolveTypes (Constant     () cons)               = pure $ Constant (getConstType cons) cons
  resolveTypes (Unary        () op expr)            = resolveTypes expr >>= \expr' -> pure $ Unary (getExpType expr') op expr'
  resolveTypes (Var          () var)                =
    do symbolTypes <- gets envSymbolTypes
       let varType = symbolTypes ! var
       when (varType /= Int) $ throwError FunctionUsedAsVariable
       pure $ Var varType var
  resolveTypes (Binary       () op expr1 expr2)     =
    do expr1' <- resolveTypes expr1
       expr2' <- resolveTypes expr2
       when (getExpType expr1' /= getExpType expr2') $ throwError IncompatibleTypes
       pure $ Binary (getExpType expr1') op expr1' expr2'
  resolveTypes (Assignment   () lhs op rhs)=
    do lhs' <- resolveTypes lhs
       rhs' <- resolveTypes rhs
       when (getExpType lhs' /= getExpType rhs') $ throwError IncompatibleTypes
       pure $ Assignment (getExpType lhs') lhs' op rhs'
  resolveTypes (Conditional  () cond conseq altern) =
    do cond' <- resolveTypes cond
       when (getExpType cond' /= Int) $ throwError IncompatibleTypes

       conseq' <- resolveTypes conseq
       altern' <- resolveTypes altern
       when (getExpType conseq' /= getExpType altern') $ throwError IncompatibleTypes

       pure $ Conditional (getExpType conseq') cond' conseq' altern'
  resolveTypes (FunctionCall () name args)          =
    do symbolTypes <- gets envSymbolTypes
       (paramsT, retT) <- case M.lookup name symbolTypes of
                            Just (Fun ps r) -> pure (ps, r)
                            _               -> throwError VariableUsedAsFunction
       args' <- mapM resolveTypes args
       let argsT = map getExpType args'
       when (paramsT /= argsT) $ throwError IncompatibleTypes
       pure $ FunctionCall retT name args'

