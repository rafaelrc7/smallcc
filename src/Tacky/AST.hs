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
  deriving (Show)

data UnaryOperator = Complement
                   | Negate
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
translateExp s (P.Constant (P.Int val)) = ([], Const val, s)
translateExp state (P.Unary op expr) = (instructions, dst, state'')
  where (exprInstructions, src, state') = translateExp state expr
        (dst, state'') = newTmpVar state'
        instruction = Unary {unaryOperator=translateUnaryOp op, unarySrc=src, unaryDst=dst}
        instructions = exprInstructions ++ [instruction]

translateUnaryOp :: P.UnaryOperator -> UnaryOperator
translateUnaryOp P.Complement = Complement
translateUnaryOp P.Negate     = Negate

newTmpVar :: State -> (Val, State)
newTmpVar State{stateLastTmp=lastTmp} = (Var newTmpLabel, State{stateLastTmp=newTmp})
  where newTmp = lastTmp + 1
        newTmpLabel = T.append "tmp." $ T.pack $ show newTmp

