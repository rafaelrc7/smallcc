{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Tacky.AST where

import           Data.Maybe (fromMaybe)
import           Data.Text  (Text)
import qualified Data.Text  as T

import qualified Parser.AST as P

data State = State { stateLastTmp   :: Int
                   , stateLastLabel :: Int
                   }

emptyState :: State
emptyState = State { stateLastTmp = 0
                   , stateLastLabel = 0
                   }

type Identifier = Text

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

translateProgram :: P.Program -> Program
translateProgram (P.Program func) = Program $ translateFunction func

translateFunction :: P.FunctionDefinition -> FunctionDefinition
translateFunction P.Function {P.funcName=name, P.funcBody=body} =
  Function { funcIdentifier=name
           , funcBody = translateStatement undefined -- body
           }

translateStatement :: P.Statement -> [Instruction]
translateStatement (P.Return expr) = expInstructions ++ [Return expVal]
  where (expInstructions, expVal, _) = translateExp emptyState expr

translateExp :: State -> P.Exp -> ([Instruction], Val, State)
translateExp s (P.Constant (P.CInt val)) = ([], Const val, s)
translateExp s (P.Unary op expr) = (instructions, dst, s'')
  where (exprInstructions, src, s') = translateExp s expr
        (dst, s'') = newTmpVar s'
        instruction = Unary {unaryOperator=translateUnaryOp op, unarySrc=src, unaryDst=dst}
        instructions = exprInstructions ++ [instruction]
translateExp s (P.Binary P.And expr1 expr2) = translateAnd s (expr1, expr2)
translateExp s (P.Binary P.Or  expr1 expr2) = translateOr  s (expr1, expr2)
translateExp s (P.Binary op exprl exprr) = (instructions, dst, s''')
  where (exprlInstructions, srcl, s') = translateExp s exprl
        (exprrInstructions, srcr, s'') = translateExp s' exprr
        (dst, s''') = newTmpVar s''
        instruction = Binary {binaryOperator=translateBinaryOp op, binarySrcs=(srcl, srcr), binaryDst=dst}
        instructions = exprlInstructions ++ exprrInstructions ++ [instruction]

translateOr :: State -> (P.Exp, P.Exp) -> ([Instruction], Val, State)
translateOr s (expr1,  expr2) = (instructions, resultVar, s''''')
  where (expr1Instructions, val1, s') = translateExp s expr1
        (expr2Instructions, val2, s'') = translateExp s' expr2
        (trueLabel, s''') = newLabel (Just "OrTrue") s''
        (endLabel, s'''') = newLabel (Just "OrEnd") s'''
        (resultVar, s''''') = newVar (Just "OrResult") s''''
        instructions = expr1Instructions
                    ++ [ JumpIfNotZero val1 trueLabel ]
                    ++ expr2Instructions
                    ++ [ JumpIfNotZero val2 trueLabel
                       , Copy {copySrc=Const 0, copyDst=resultVar}
                       , Jump endLabel
                       , Label trueLabel
                       , Copy {copySrc=Const 1, copyDst=resultVar}
                       , Label endLabel
                       ]

translateAnd :: State -> (P.Exp, P.Exp) -> ([Instruction], Val, State)
translateAnd s (expr1,  expr2) = (instructions, resultVar, s''''')
  where (expr1Instructions, val1, s') = translateExp s expr1
        (expr2Instructions, val2, s'') = translateExp s' expr2
        (falseLabel, s''') = newLabel (Just "AndFalse") s''
        (endLabel, s'''') = newLabel (Just "AndEnd") s'''
        (resultVar, s''''') = newVar (Just "AndResult") s''''
        instructions = expr1Instructions
                    ++ [ JumpIfZero val1 falseLabel ]
                    ++ expr2Instructions
                    ++ [ JumpIfZero val2 falseLabel
                       , Copy {copySrc=Const 1, copyDst=resultVar}
                       , Jump endLabel
                       , Label falseLabel
                       , Copy {copySrc=Const 0, copyDst=resultVar}
                       , Label endLabel
                       ]

translateUnaryOp :: P.UnaryOperator -> UnaryOperator
translateUnaryOp P.Complement = Complement
translateUnaryOp P.Negate     = Negate
translateUnaryOp P.Not        = Not

translateBinaryOp :: P.BinaryOperator -> BinaryOperator
translateBinaryOp P.BitOr          = BitOr
translateBinaryOp P.BitXOR         = BitXOR
translateBinaryOp P.BitAnd         = BitAnd
translateBinaryOp P.EqualsTo       = EqualsTo
translateBinaryOp P.NotEqualsTo    = NotEqualsTo
translateBinaryOp P.Less           = Less
translateBinaryOp P.LessOrEqual    = LessOrEqual
translateBinaryOp P.Greater        = Greater
translateBinaryOp P.GreaterOrEqual = GreaterOrEqual
translateBinaryOp P.BitShiftLeft   = BitShiftLeft
translateBinaryOp P.BitShiftRight  = BitShiftRight
translateBinaryOp P.Add            = Add
translateBinaryOp P.Subtract       = Subtract
translateBinaryOp P.Multiply       = Multiply
translateBinaryOp P.Divide         = Divide
translateBinaryOp P.Remainder      = Remainder

translateBinaryOp P.And            = undefined
translateBinaryOp P.Or             = undefined

newVar :: Maybe Text -> State -> (Val, State)
newVar label state@State{ stateLastTmp = lastTmp } = (Var newTmpLabel, state{ stateLastTmp = newTmp })
  where varLabel = T.snoc (fromMaybe "tmp" label) '.'
        newTmp = lastTmp + 1
        newTmpLabel = T.append varLabel $ T.pack $ show newTmp

newTmpVar :: State -> (Val, State)
newTmpVar = newVar Nothing

newLabel :: Maybe Text -> State -> (Identifier, State)
newLabel caption state = (label, state{ stateLastLabel = labelNumber })
  where labelNumber = stateLastLabel state + 1
        labelNumberText = T.pack $ show labelNumber
        label = case caption of Nothing -> labelNumberText
                                Just labelCaption -> labelCaption `T.append` labelNumberText

