{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Tacky.AST where

import           Data.Text  (Text)
import qualified Data.Text  as T

import qualified Parser.AST as P

newtype State = State {
  stateLastTmp :: Int
}

emptyState :: State
emptyState = State {stateLastTmp=0}

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
  deriving (Show)

data UnaryOperator = Complement
                   | Negate
  deriving (Show)

data BinaryOperator = Add
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
           , funcBody = translateStatement body
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
translateExp s (P.Binary op exprl exprr) = (instructions, dst, s''')
  where (exprlInstructions, srcl, s') = translateExp s exprl
        (exprrInstructions, srcr, s'') = translateExp s' exprr
        (dst, s''') = newTmpVar s''
        instruction = Binary {binaryOperator=translateBinaryOp op, binarySrcs=(srcl, srcr), binaryDst=dst}
        instructions = exprlInstructions ++ exprrInstructions ++ [instruction]

translateUnaryOp :: P.UnaryOperator -> UnaryOperator
translateUnaryOp P.Complement = Complement
translateUnaryOp P.Negate     = Negate

translateBinaryOp :: P.BinaryOperator -> BinaryOperator
translateBinaryOp P.Add       = Add
translateBinaryOp P.Subtract  = Subtract
translateBinaryOp P.Multiply  = Multiply
translateBinaryOp P.Divide    = Divide
translateBinaryOp P.Remainder = Remainder

newTmpVar :: State -> (Val, State)
newTmpVar State{stateLastTmp=lastTmp} = (Var newTmpLabel, State{stateLastTmp=newTmp})
  where newTmp = lastTmp + 1
        newTmpLabel = T.append "tmp." $ T.pack $ show newTmp

