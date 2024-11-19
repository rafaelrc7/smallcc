{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE OverloadedStrings      #-}

module Tacky.AST where

import           Data.Maybe                             (fromMaybe)
import           Data.Text                              (Text)
import qualified Data.Text                              as T

import           Control.Monad                          (void)
import           Control.Monad.State                    (StateT, evalStateT,
                                                         gets, modify)
import           Control.Monad.Writer                   (MonadWriter (tell),
                                                         Writer, runWriter)
import           Data.Functor                           (($>))
import           Numeric.Natural                        (Natural)
import           Parser.AST                             (Identifier)
import qualified Parser.AST                             as P
import           SemanticAnalyzer.SemanticAnalyzerMonad (LabelResolvingPhase)

newtype Program = Program FunctionDefinition
  deriving (Show)

data FunctionDefinition = Function { funcIdentifier :: Identifier
                                   , funcBody       :: [Instruction]
                                   }
  deriving (Show)

data Instruction = Return Val
                 | Unary { unaryOperator :: UnaryOperator
                         , unarySrc      :: Val
                         , unaryDst      :: Val
                         }
                 | Binary { binaryOperator :: BinaryOperator
                          , binarySrcs     :: (Val, Val)
                          , binaryDst      :: Val
                          }
                 | Copy { copySrc :: Val
                        , copyDst :: Val
                        }
                 | Jump Identifier
                 | JumpIfZero Val Identifier
                 | JumpIfNotZero Val Identifier
                 | Label Identifier
  deriving (Show)

data UnaryOperator = Complement
                   | Negate
                   | Not
  deriving (Show)

data BinaryOperator = BitOr
                    | BitXOR
                    | BitAnd
                    | EqualsTo
                    | NotEqualsTo
                    | Less
                    | LessOrEqual
                    | Greater
                    | GreaterOrEqual
                    | BitShiftLeft
                    | BitShiftRight
                    | Add
                    | Subtract
                    | Multiply
                    | Divide
                    | Remainder
  deriving (Show)

data Val = Const Int
         | Var Identifier
  deriving (Show)

data Environment = Environment { envLastVar   :: Natural
                               , envLastLabel :: Natural
                               }

emptyEnv :: Environment
emptyEnv = Environment { envLastVar = 0, envLastLabel = 0 }

type TackyGenerationMonad a = StateT Environment (Writer [Instruction]) a

newVar :: Maybe Text -> TackyGenerationMonad Val
newVar caption = gets envLastVar >>= \lastVar ->
  let currVar = succ lastVar
      currVarText = T.pack $ show currVar
      var = fromMaybe "tmp" caption <> "." <> currVarText
  in modify (\s -> s { envLastVar = currVar }) $> Var var

newTmpVar :: TackyGenerationMonad Val
newTmpVar = newVar Nothing

newLabel :: Maybe Text -> TackyGenerationMonad Text
newLabel caption = gets envLastLabel >>= \lastLabel ->
  let currLabel = succ lastLabel
      currLabelText = T.pack $ show currLabel
      label = case caption of
                Nothing       -> "." <> currLabelText
                Just caption' -> caption' <> "." <> currLabelText
  in modify (\s -> s { envLastLabel = currLabel }) $> label

translateProgram :: P.Program LabelResolvingPhase -> Program
translateProgram (P.Program func) = Program $ translateFunction func

translateFunction :: P.FunctionDefinition LabelResolvingPhase -> FunctionDefinition
translateFunction (P.Function name body) =
     Function { funcIdentifier = name
              , funcBody = body' ++ [ Return (Const 0) ]
              }
   where env = emptyEnv
         body' = snd . runWriter $ evalStateT (translate body) env

class TackyGenerator a b | a -> b where
  translate :: a -> TackyGenerationMonad b

instance TackyGenerator (P.Block LabelResolvingPhase) () where
  translate :: P.Block LabelResolvingPhase -> TackyGenerationMonad ()
  translate (P.Block blockItens) = mapM_ translate blockItens

instance TackyGenerator (P.BlockItem LabelResolvingPhase) () where
  translate :: P.BlockItem LabelResolvingPhase -> TackyGenerationMonad ()
  translate (P.BlockStatement stmt)    = translate stmt
  translate (P.BlockDeclaration  decl) = translate decl

instance TackyGenerator (P.Statement LabelResolvingPhase) () where
  translate :: P.Statement LabelResolvingPhase -> TackyGenerationMonad ()
  translate (P.LabeledStatement label stmt) = translate label >> translate stmt
  translate (P.UnlabeledStatement stmt)     = translate stmt

instance TackyGenerator (P.UnlabeledStatement LabelResolvingPhase) () where
  translate :: P.UnlabeledStatement LabelResolvingPhase -> TackyGenerationMonad ()
  translate (P.Null _)      = pure ()
  translate (P.Return _ expr) = translate expr >>= \v -> tell [ Return v ]
  translate (P.Expression _ expr) = void $ translate expr
  translate (P.Goto _ label) = tell [ Jump label ]
  translate (P.Compound _ block) = translate block
  translate (P.Switch _ _ _) = undefined
  translate (P.If _ cond conse Nothing) =
    do condVal <- translate cond
       endLabel <- newLabel (Just "end")
       tell [ JumpIfZero condVal endLabel ]
       translate conse
       tell [ Label endLabel ]
  translate (P.If _ cond conse (Just altern)) =
    do condVal <- translate cond
       alternLabel <- newLabel (Just "else")
       tell [ JumpIfZero condVal alternLabel ]
       translate conse
       endLabel <- newLabel (Just "end")
       tell [ Jump endLabel
            , Label alternLabel
            ]
       translate altern
       tell [ Label endLabel ]
  translate (P.DoWhile label body cond) =
    do let startLabel    = label <> "_start"
       let continueLabel = label <> "_continue"
       let breakLabel    = label <> "_break"
       tell [ Label startLabel ]
       translate body
       tell [ Label continueLabel ]
       condVal <- translate cond
       tell [ JumpIfNotZero condVal startLabel ]
       tell [ Label breakLabel ]
  translate (P.While label cond body) =
    do let continueLabel = label <> "_continue"
       let breakLabel    = label <> "_break"
       tell [ Label continueLabel ]
       condVal <- translate cond
       tell [ JumpIfZero condVal breakLabel ]
       translate body
       tell [ Jump continueLabel
            , Label breakLabel
            ]
  translate (P.For label ini cond post body) =
    do let startLabel    = label <> "_start"
       let continueLabel = label <> "_continue"
       let breakLabel    = label <> "_break"
       translate ini
       tell [ Label startLabel ]
       condVal <- case cond of
         Just cond' -> translate cond'
         Nothing    -> pure $ Const 1
       tell [ JumpIfZero condVal breakLabel ]
       translate body
       tell [ Label continueLabel ]
       case post of
         Just post' -> void $ translate post'
         Nothing    -> pure ()
       tell [ Jump startLabel
            , Label breakLabel
            ]
  translate (P.Break label)    = tell [ Jump $ label <> "_break" ]
  translate (P.Continue label) = tell [ Jump $ label <> "_continue" ]

instance TackyGenerator (P.ForInit LabelResolvingPhase) () where
  translate :: P.ForInit LabelResolvingPhase -> TackyGenerationMonad ()
  translate (P.InitExp expr)  = case expr of
    Just expr' -> void $ translate expr'
    Nothing    -> pure ()
  translate (P.InitDecl decl) = translate decl

instance TackyGenerator (P.Label LabelResolvingPhase) () where
  translate (P.Label _ label) = tell [ Label label ]
  translate (P.Case _ _)      = tell [ ]
  translate (P.Default _)     = tell [ ]

instance TackyGenerator (P.Declaration LabelResolvingPhase) () where
  translate :: P.Declaration LabelResolvingPhase -> TackyGenerationMonad ()
  translate (P.Declaration _ _ Nothing) = pure ()
  translate (P.Declaration _ lhs (Just rhs)) =
    do rhsVal <- translate rhs
       tell [ Copy { copySrc = rhsVal
                   , copyDst = Var lhs
                   }
            ]

instance TackyGenerator (P.Exp LabelResolvingPhase) Val where
  translate :: P.Exp LabelResolvingPhase -> TackyGenerationMonad Val
  translate (P.Constant _ (P.CInt val)) = pure $ Const val
  translate (P.Var _ var) = pure $ Var var
  translate (P.Conditional _ cond conse altern) =
      do condVal <- translate cond
         result <- newVar (Just "result")
         alternLabel <- newLabel (Just "alternative")
         tell [ JumpIfZero condVal alternLabel ]
         conseVal <- translate conse
         tell $ copyToResult conseVal result
         endLabel <- newLabel (Just "end")
         tell [ Jump endLabel
              , Label alternLabel
              ]
         alternVal <- translate altern
         tell $ copyToResult alternVal result
         tell [ Label endLabel ]
         pure result
    where copyToResult from to =
            [ Copy { copySrc = from
                   , copyDst = to
                   }
            ]

  translate (P.Unary _ P.Complement expr) = translateUnaryExp Complement expr
  translate (P.Unary _ P.Negate     expr) = translateUnaryExp Negate     expr
  translate (P.Unary _ P.Not        expr) = translateUnaryExp Not        expr
  translate (P.Unary _ (P.UnaryAssignmentOperator P.PreDecrement)  var) = translatePreAssignmentExp Subtract var
  translate (P.Unary _ (P.UnaryAssignmentOperator P.PreIncrement)  var) = translatePreAssignmentExp Add var
  translate (P.Unary _ (P.UnaryAssignmentOperator P.PostDecrement) var) = translatePostAssignmentExp Subtract var
  translate (P.Unary _ (P.UnaryAssignmentOperator P.PostIncrement) var) = translatePostAssignmentExp Add var

  translate (P.Binary _ P.And expr1 expr2) = translateAndExp expr1 expr2
  translate (P.Binary _ P.Or  expr1 expr2) = translateOrExp  expr1 expr2

  translate (P.Binary _ P.BitOr          expr1 expr2) = translateBinaryExp BitOr          expr1 expr2
  translate (P.Binary _ P.BitXOR         expr1 expr2) = translateBinaryExp BitXOR         expr1 expr2
  translate (P.Binary _ P.BitAnd         expr1 expr2) = translateBinaryExp BitAnd         expr1 expr2
  translate (P.Binary _ P.EqualsTo       expr1 expr2) = translateBinaryExp EqualsTo       expr1 expr2
  translate (P.Binary _ P.NotEqualsTo    expr1 expr2) = translateBinaryExp NotEqualsTo    expr1 expr2
  translate (P.Binary _ P.Less           expr1 expr2) = translateBinaryExp Less           expr1 expr2
  translate (P.Binary _ P.LessOrEqual    expr1 expr2) = translateBinaryExp LessOrEqual    expr1 expr2
  translate (P.Binary _ P.Greater        expr1 expr2) = translateBinaryExp Greater        expr1 expr2
  translate (P.Binary _ P.GreaterOrEqual expr1 expr2) = translateBinaryExp GreaterOrEqual expr1 expr2
  translate (P.Binary _ P.BitShiftLeft   expr1 expr2) = translateBinaryExp BitShiftLeft   expr1 expr2
  translate (P.Binary _ P.BitShiftRight  expr1 expr2) = translateBinaryExp BitShiftRight  expr1 expr2
  translate (P.Binary _ P.Add            expr1 expr2) = translateBinaryExp Add            expr1 expr2
  translate (P.Binary _ P.Subtract       expr1 expr2) = translateBinaryExp Subtract       expr1 expr2
  translate (P.Binary _ P.Multiply       expr1 expr2) = translateBinaryExp Multiply       expr1 expr2
  translate (P.Binary _ P.Divide         expr1 expr2) = translateBinaryExp Divide         expr1 expr2
  translate (P.Binary _ P.Remainder      expr1 expr2) = translateBinaryExp Remainder      expr1 expr2

  translate (P.Assignment _ lhs P.AddAssign           rhs) = translateAssignmentExp Add           lhs rhs
  translate (P.Assignment _ lhs P.SubAssign           rhs) = translateAssignmentExp Subtract      lhs rhs
  translate (P.Assignment _ lhs P.MulAssign           rhs) = translateAssignmentExp Multiply      lhs rhs
  translate (P.Assignment _ lhs P.DivAssign           rhs) = translateAssignmentExp Divide        lhs rhs
  translate (P.Assignment _ lhs P.RemAssign           rhs) = translateAssignmentExp Remainder     lhs rhs
  translate (P.Assignment _ lhs P.BitAndAssign        rhs) = translateAssignmentExp BitAnd        lhs rhs
  translate (P.Assignment _ lhs P.BitOrAssign         rhs) = translateAssignmentExp BitOr         lhs rhs
  translate (P.Assignment _ lhs P.BitXORAssign        rhs) = translateAssignmentExp BitXOR        lhs rhs
  translate (P.Assignment _ lhs P.BitShiftLeftAssign  rhs) = translateAssignmentExp BitShiftLeft  lhs rhs
  translate (P.Assignment _ lhs P.BitShiftRightAssign rhs) = translateAssignmentExp BitShiftRight lhs rhs

  translate (P.Assignment _ lhs P.Assign rhs) =
    do lhs' <- translate lhs
       rhs' <- translate rhs
       tell [ Copy { copySrc = rhs', copyDst = lhs' } ]
       pure lhs'

translateUnaryExp :: UnaryOperator -> P.Exp LabelResolvingPhase -> TackyGenerationMonad Val
translateUnaryExp op expr =
  do src <- translate expr
     dst <- newTmpVar
     tell [ Unary { unaryOperator = op, unarySrc = src, unaryDst = dst } ]
     pure dst

translatePreAssignmentExp :: BinaryOperator -> P.Exp LabelResolvingPhase -> TackyGenerationMonad Val
translatePreAssignmentExp op var =
  do var' <- translate var
     tell [ Binary { binaryOperator = op, binarySrcs = (var', Const 1), binaryDst = var' } ]
     pure var'

translatePostAssignmentExp :: BinaryOperator -> P.Exp LabelResolvingPhase -> TackyGenerationMonad Val
translatePostAssignmentExp op var =
  do var' <- translate var
     tmp <- newTmpVar
     tell [ Copy { copySrc = var'
                 , copyDst = tmp
                 }
          , Binary { binaryOperator = op
                   , binarySrcs = (var', Const 1)
                   , binaryDst = var'
                   }
          ]
     pure tmp

translateAndExp :: P.Exp LabelResolvingPhase -> P.Exp LabelResolvingPhase -> TackyGenerationMonad Val
translateAndExp expr1 expr2 =
  do val1 <- translate expr1
     falseLabel <- newLabel (Just "and_false")
     tell [ JumpIfZero val1 falseLabel ]
     val2 <- translate expr2
     result <- newVar (Just "and_result")
     endLabel <- newLabel (Just "and_end")
     tell [ JumpIfZero val2 falseLabel
          , Copy { copySrc = Const 1, copyDst = result }
          , Jump endLabel
          , Label falseLabel
          , Copy { copySrc = Const 0, copyDst = result }
          , Label endLabel
          ]
     pure result

translateOrExp :: P.Exp LabelResolvingPhase -> P.Exp LabelResolvingPhase -> TackyGenerationMonad Val
translateOrExp expr1 expr2 =
  do val1 <- translate expr1
     trueLabel <- newLabel (Just "or_true")
     tell [ JumpIfNotZero val1 trueLabel ]
     val2 <- translate expr2
     result <- newVar (Just "or_result")
     endLabel <- newLabel (Just "end_label")
     tell [ JumpIfNotZero val2 trueLabel
          , Copy { copySrc = Const 0, copyDst = result }
          , Jump endLabel
          , Label trueLabel
          , Copy { copySrc = Const 1, copyDst = result }
          , Label endLabel
          ]
     pure result

translateBinaryExp :: BinaryOperator -> P.Exp LabelResolvingPhase -> P.Exp LabelResolvingPhase -> TackyGenerationMonad Val
translateBinaryExp op expr1 expr2 =
  do src1 <- translate expr1
     src2 <- translate expr2
     dst  <- newTmpVar
     tell [ Binary { binaryOperator = op, binarySrcs = (src1, src2), binaryDst = dst } ]
     pure dst

translateAssignmentExp :: BinaryOperator -> P.Exp LabelResolvingPhase -> P.Exp LabelResolvingPhase -> TackyGenerationMonad Val
translateAssignmentExp op lhs rhs =
  do lhs' <- translate lhs
     rhs' <- translate rhs
     tmp  <- newTmpVar
     tell [ Binary { binaryOperator = op, binarySrcs = (lhs', rhs'), binaryDst = tmp }
          , Copy { copySrc = tmp, copyDst = lhs' }
          ]
     pure lhs'

