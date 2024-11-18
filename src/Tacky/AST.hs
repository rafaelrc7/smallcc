{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE OverloadedStrings      #-}

module Tacky.AST where

import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Control.Monad        (void)
import           Control.Monad.State  (StateT, evalStateT, gets, modify)
import           Control.Monad.Writer (MonadWriter (tell), Writer, runWriter)
import           Data.Functor         (($>))
import           Numeric.Natural      (Natural)
import           Parser.AST           (Identifier)
import qualified Parser.AST           as P
import qualified SemanticAnalyzer.AST as SA

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

translateProgram :: SA.Program -> Program
translateProgram (SA.Program func) = Program $ translateFunction func

translateFunction :: SA.FunctionDefinition -> FunctionDefinition
translateFunction (SA.Function name body) =
     Function { funcIdentifier = name
              , funcBody = body' ++ [ Return (Const 0) ]
              }
   where env = emptyEnv
         body' = snd . runWriter $ evalStateT (translate body) env

class TackyGenerator a b | a -> b where
  translate :: a -> TackyGenerationMonad b

instance TackyGenerator SA.Block () where
  translate :: SA.Block -> TackyGenerationMonad ()
  translate (SA.Block blockItens) = mapM_ translate blockItens

instance TackyGenerator SA.BlockItem () where
  translate :: SA.BlockItem -> TackyGenerationMonad ()
  translate (SA.Stmt stmt) = translate stmt
  translate (SA.Dec  decl) = translate decl

instance TackyGenerator SA.Statement () where
  translate :: SA.Statement -> TackyGenerationMonad ()
  translate SA.Null          = pure ()
  translate (SA.Return expr) = translate expr >>= \v -> tell [ Return v ]
  translate (SA.Expression expr) = void $ translate expr
  translate (SA.Goto label) = tell [ Jump label ]
  translate (SA.Label label) = tell [ Label label ]
  translate (SA.Compound block) = translate block
  translate (SA.If cond conse Nothing) =
    do condVal <- translate cond
       endLabel <- newLabel (Just "end")
       tell [ JumpIfZero condVal endLabel ]
       translate conse
       tell [ Label endLabel ]
  translate (SA.If cond conse (Just altern)) =
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

instance TackyGenerator SA.Declaration () where
  translate :: SA.Declaration -> TackyGenerationMonad ()
  translate (SA.Declaration _ Nothing) = pure ()
  translate (SA.Declaration lhs (Just rhs)) =
    do rhsVal <- translate rhs
       tell [ Copy { copySrc = rhsVal
                   , copyDst = Var lhs
                   }
            ]

instance TackyGenerator SA.Exp Val where
  translate :: SA.Exp -> TackyGenerationMonad Val
  translate (SA.Constant (P.CInt val)) = pure $ Const val
  translate (SA.Var var) = pure $ Var var
  translate (SA.Conditional cond conse altern) =
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

  translate (SA.Unary P.Complement expr) = translateUnaryExp Complement expr
  translate (SA.Unary P.Negate     expr) = translateUnaryExp Negate     expr
  translate (SA.Unary P.Not        expr) = translateUnaryExp Not        expr
  translate (SA.Unary (P.UnaryAssignmentOperator P.PreDecrement)  var) = translatePreAssignmentExp Subtract var
  translate (SA.Unary (P.UnaryAssignmentOperator P.PreIncrement)  var) = translatePreAssignmentExp Add var
  translate (SA.Unary (P.UnaryAssignmentOperator P.PostDecrement) var) = translatePostAssignmentExp Subtract var
  translate (SA.Unary (P.UnaryAssignmentOperator P.PostIncrement) var) = translatePostAssignmentExp Add var

  translate (SA.Binary P.And expr1 expr2) = translateAndExp expr1 expr2
  translate (SA.Binary P.Or  expr1 expr2) = translateOrExp  expr1 expr2

  translate (SA.Binary P.BitOr          expr1 expr2) = translateBinaryExp BitOr          expr1 expr2
  translate (SA.Binary P.BitXOR         expr1 expr2) = translateBinaryExp BitXOR         expr1 expr2
  translate (SA.Binary P.BitAnd         expr1 expr2) = translateBinaryExp BitAnd         expr1 expr2
  translate (SA.Binary P.EqualsTo       expr1 expr2) = translateBinaryExp EqualsTo       expr1 expr2
  translate (SA.Binary P.NotEqualsTo    expr1 expr2) = translateBinaryExp NotEqualsTo    expr1 expr2
  translate (SA.Binary P.Less           expr1 expr2) = translateBinaryExp Less           expr1 expr2
  translate (SA.Binary P.LessOrEqual    expr1 expr2) = translateBinaryExp LessOrEqual    expr1 expr2
  translate (SA.Binary P.Greater        expr1 expr2) = translateBinaryExp Greater        expr1 expr2
  translate (SA.Binary P.GreaterOrEqual expr1 expr2) = translateBinaryExp GreaterOrEqual expr1 expr2
  translate (SA.Binary P.BitShiftLeft   expr1 expr2) = translateBinaryExp BitShiftLeft   expr1 expr2
  translate (SA.Binary P.BitShiftRight  expr1 expr2) = translateBinaryExp BitShiftRight  expr1 expr2
  translate (SA.Binary P.Add            expr1 expr2) = translateBinaryExp Add            expr1 expr2
  translate (SA.Binary P.Subtract       expr1 expr2) = translateBinaryExp Subtract       expr1 expr2
  translate (SA.Binary P.Multiply       expr1 expr2) = translateBinaryExp Multiply       expr1 expr2
  translate (SA.Binary P.Divide         expr1 expr2) = translateBinaryExp Divide         expr1 expr2
  translate (SA.Binary P.Remainder      expr1 expr2) = translateBinaryExp Remainder      expr1 expr2

  translate (SA.Assignment lhs P.AddAssign           rhs) = translateAssignmentExp Add           lhs rhs
  translate (SA.Assignment lhs P.SubAssign           rhs) = translateAssignmentExp Subtract      lhs rhs
  translate (SA.Assignment lhs P.MulAssign           rhs) = translateAssignmentExp Multiply      lhs rhs
  translate (SA.Assignment lhs P.DivAssign           rhs) = translateAssignmentExp Divide        lhs rhs
  translate (SA.Assignment lhs P.RemAssign           rhs) = translateAssignmentExp Remainder     lhs rhs
  translate (SA.Assignment lhs P.BitAndAssign        rhs) = translateAssignmentExp BitAnd        lhs rhs
  translate (SA.Assignment lhs P.BitOrAssign         rhs) = translateAssignmentExp BitOr         lhs rhs
  translate (SA.Assignment lhs P.BitXORAssign        rhs) = translateAssignmentExp BitXOR        lhs rhs
  translate (SA.Assignment lhs P.BitShiftLeftAssign  rhs) = translateAssignmentExp BitShiftLeft  lhs rhs
  translate (SA.Assignment lhs P.BitShiftRightAssign rhs) = translateAssignmentExp BitShiftRight lhs rhs

  translate (SA.Assignment lhs P.Assign rhs) =
    do lhs' <- translate lhs
       rhs' <- translate rhs
       tell [ Copy { copySrc = rhs', copyDst = lhs' } ]
       pure lhs'

translateUnaryExp :: UnaryOperator -> SA.Exp -> TackyGenerationMonad Val
translateUnaryExp op expr =
  do src <- translate expr
     dst <- newTmpVar
     tell [ Unary { unaryOperator = op, unarySrc = src, unaryDst = dst } ]
     pure dst

translatePreAssignmentExp :: BinaryOperator -> SA.Exp -> TackyGenerationMonad Val
translatePreAssignmentExp op var =
  do var' <- translate var
     tell [ Binary { binaryOperator = op, binarySrcs = (var', Const 1), binaryDst = var' } ]
     pure var'

translatePostAssignmentExp :: BinaryOperator -> SA.Exp -> TackyGenerationMonad Val
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

translateAndExp :: SA.Exp -> SA.Exp -> TackyGenerationMonad Val
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

translateOrExp :: SA.Exp -> SA.Exp -> TackyGenerationMonad Val
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

translateBinaryExp :: BinaryOperator -> SA.Exp -> SA.Exp -> TackyGenerationMonad Val
translateBinaryExp op expr1 expr2 =
  do src1 <- translate expr1
     src2 <- translate expr2
     dst  <- newTmpVar
     tell [ Binary { binaryOperator = op, binarySrcs = (src1, src2), binaryDst = dst } ]
     pure dst

data State' = State { stateLastTmp  :: Int
                   , stateLastLabel :: Int
                   }

translateAssignmentExp :: BinaryOperator -> SA.Exp -> SA.Exp -> TackyGenerationMonad Val
translateAssignmentExp op lhs rhs =
  do lhs' <- translate lhs
     rhs' <- translate rhs
     tmp  <- newTmpVar
     tell [ Binary { binaryOperator = op, binarySrcs = (lhs', rhs'), binaryDst = tmp }
          , Copy { copySrc = tmp, copyDst = lhs' }
          ]
     pure lhs'

