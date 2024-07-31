module AssemblyGenerator.AST where

import           Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)

import qualified Tacky.AST       as T

type Identifier = Text

newtype Program = Program FunctionDefinition
  deriving (Show)

data FunctionDefinition = Function { funcName         :: Identifier
                                   , funcInstructions :: [Instruction]
                                   }
  deriving (Show)

data Instruction = Mov { movSrc :: Operand
                       , movDst :: Operand
                       }
                 | Unary { unaryOp      :: UnaryOperator
                         , unaryOperand :: Operand
                         }
                 | AllocateStack Int
                 | Ret
  deriving (Show)

data UnaryOperator = Neg
                   | Not
  deriving (Show)

data Operand = Imm Int
             | Reg Reg
             | Pseudo Identifier
             | Stack Int
  deriving (Show)

data Reg = AX
         | R10
  deriving (Show)

replacePseudoRegisters :: Program -> (Program, Int)
replacePseudoRegisters program = (program', lastOffsetVarMap varMap''')
  where (program', varMap''') = replaceInProgram newVarMap program

        replaceInProgram :: VarMap -> Program -> (Program, VarMap)
        replaceInProgram varMap (Program function) = (Program function', varMap')
          where (function', varMap') = replaceInFunction varMap function

        replaceInFunction :: VarMap -> FunctionDefinition -> (FunctionDefinition, VarMap)
        replaceInFunction varMap (Function {funcName=name, funcInstructions=instructions}) = (Function {funcName=name, funcInstructions=instructions'}, varMap')
          where (instructions', varMap') = foldr (\i (is, vm) -> let (i', vm') = replaceInInstruction vm i in (i' : is, vm'))
                                                 ([], varMap)
                                                 instructions

        replaceInInstruction :: VarMap -> Instruction -> (Instruction, VarMap)
        replaceInInstruction varMap Mov {movSrc=src, movDst=dst} = (Mov {movSrc=src', movDst=dst'}, varMap'')
          where (src', varMap')  = replaceInOperand varMap src
                (dst', varMap'') = replaceInOperand varMap' dst
        replaceInInstruction varMap Unary {unaryOp=op, unaryOperand=operand} = (Unary {unaryOp=op, unaryOperand=operand'}, varMap')
          where (operand', varMap')  = replaceInOperand varMap operand
        replaceInInstruction varMap instruction = (instruction, varMap)

        replaceInOperand :: VarMap -> Operand -> (Operand, VarMap)
        replaceInOperand varMap (Pseudo identifier) = (Stack offset, varMap')
          where (varMap', offset) = fetchVarMap varMap identifier
        replaceInOperand varMap operand = (operand, varMap)

fixInstructions :: (Program, Int) -> Program
fixInstructions (program, lastOffset) = fixProgram program
  where fixProgram :: Program -> Program
        fixProgram (Program function) = Program $ fixFunction function

        fixFunction :: FunctionDefinition -> FunctionDefinition
        fixFunction (Function {funcName=name, funcInstructions=instructions}) = Function {funcName=name, funcInstructions=instructions'}
          where instructions' = AllocateStack lastOffset : concatMap fixInstruction instructions


        fixInstruction :: Instruction -> [Instruction]
        fixInstruction Mov {movSrc=Stack src, movDst=Stack dst} = [ Mov {movSrc=Stack src, movDst=Reg R10}
                                                                  , Mov {movSrc=Reg R10, movDst=Stack dst}
                                                                  ]
        fixInstruction i = [i]

type VarMap = (Map Identifier Int, Int)

varSize :: Int
varSize = 4

newVarMap :: VarMap
newVarMap = (Map.empty, 0)

lastOffsetVarMap :: VarMap -> Int
lastOffsetVarMap = abs . snd

fetchVarMap :: VarMap -> Identifier -> (VarMap, Int)
fetchVarMap vm@(varMap, lastOffset) identifier = case varMap !? identifier of
                                                   Just offset -> (vm, offset)
                                                   Nothing -> ((varMap', newOffset), newOffset)
  where newOffset = lastOffset - varSize
        varMap' = Map.insert identifier newOffset varMap

translateProgram :: T.Program -> Program
translateProgram (T.Program func) = Program $ translateFunctionDefinition func

translateFunctionDefinition :: T.FunctionDefinition -> FunctionDefinition
translateFunctionDefinition func = Function { funcName = T.funcIdentifier func
                                            , funcInstructions = translateInstruction $ T.funcBody func
                                            }

translateInstruction :: [T.Instruction] -> [Instruction]
translateInstruction [] = []
translateInstruction (T.Return expr : is) = [ Mov {movSrc=translateOperand expr, movDst=Reg AX}
                                            , Ret
                                            ] ++ translateInstruction is
translateInstruction (T.Unary { T.unaryOperator=op, T.unarySrc=src, T.unaryDst=dst} : is) =
  [ Mov {movSrc=src', movDst=dst'}
  , Unary {unaryOp=op', unaryOperand=dst'}
  ] ++ translateInstruction is
  where src' = translateOperand src
        dst' = translateOperand dst
        op' = translateUnaryOp op

translateOperand :: T.Val -> Operand
translateOperand (T.Const v) = Imm v
translateOperand (T.Var i)   = Pseudo i

translateUnaryOp :: T.UnaryOperator -> UnaryOperator
translateUnaryOp T.Complement = Not
translateUnaryOp T.Negate     = Neg

