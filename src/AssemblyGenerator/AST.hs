module AssemblyGenerator.AST where

import qualified Data.Text  as T

import qualified Parser.AST as P

newtype Program = Program FunctionDefinition
  deriving (Show)

data FunctionDefinition = Function { funcName         :: T.Text
                                   , funcInstructions :: [Instruction]
                                   }
  deriving (Show)

data Instruction = Mov { movSrc :: Operand
                       , movDst :: Operand
                       }
                 | Ret
  deriving (Show)

data Operand = Imm Int
             | Register
  deriving (Show)

translateProgram :: P.Program -> Program
translateProgram (P.Program func) = Program $ translateFunctionDefinition func

translateFunctionDefinition :: P.FunctionDefinition -> FunctionDefinition
translateFunctionDefinition func = Function { funcName = P.funcName func
                                            , funcInstructions = translateInstruction $ P.funcBody func
                                            }

translateInstruction :: P.Statement -> [Instruction]
translateInstruction (P.Return expr) = [ Mov {movSrc=translateOperand expr, movDst=Register}
                                       , Ret
                                       ]

translateOperand :: P.Exp -> Operand
translateOperand (P.Constant (P.Int v)) = Imm v

