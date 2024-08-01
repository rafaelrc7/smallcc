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
                 | Binary { binaryOp       :: BinaryOperator
                          , binaryOperands :: (Operand, Operand)
                          }
                 | Idiv Operand
                 | Cdq
                 | AllocateStack Int
                 | Ret
  deriving (Show)

data UnaryOperator = Neg
                   | Not
  deriving (Show)

data BinaryOperator = Add
                    | Sub
                    | Mult
                    | And
                    | Or
                    | Xor
                    | ShiftLeft
                    | ShiftRight
  deriving (Show)

data Operand = Imm Int
             | Reg Reg
             | Pseudo Identifier
             | Stack Int
  deriving (Show)

data Reg = AX
         | DX
         | CL
         | CX
         | R10
         | R11
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
        replaceInInstruction varMap Binary {binaryOp=op, binaryOperands=(operandl, operandr)} = (Binary {binaryOp=op, binaryOperands=operands}, varMap'')
          where (operandl', varMap')  = replaceInOperand varMap operandl
                (operandr', varMap'')  = replaceInOperand varMap' operandr
                operands = (operandl', operandr')
        replaceInInstruction varMap (Idiv operand) = (Idiv operand', varMap')
          where (operand', varMap') = replaceInOperand varMap operand
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
        fixInstruction Mov {movSrc=src@(Stack _), movDst=dst@(Stack _)} = [ Mov {movSrc=src, movDst=Reg R10}
                                                                          , Mov {movSrc=Reg R10, movDst=dst}
                                                                          ]
        fixInstruction (Idiv imm@(Imm _)) = [ Mov {movSrc=imm, movDst=Reg R10}
                                            , Idiv (Reg R10)
                                            ]
        fixInstruction Binary {binaryOp=op@Mult, binaryOperands=(operandl, operandr@(Stack _))} = [ Mov {movSrc=operandr, movDst=Reg R11}
                                                                                                  , Binary {binaryOp=op, binaryOperands=(operandl, Reg R11)}
                                                                                                  , Mov {movSrc=Reg R11, movDst=operandr}
                                                                                                  ]
        fixInstruction Binary {binaryOp=op@ShiftLeft, binaryOperands=(operandl@(Stack _), operandr)} = [ Mov {movSrc=operandl, movDst=Reg CX}
                                                                                                       , Binary {binaryOp=op, binaryOperands=(Reg CL, operandr)}
                                                                                                       ]
        fixInstruction Binary {binaryOp=op@ShiftRight, binaryOperands=(operandl@(Stack _), operandr)} = [ Mov {movSrc=operandl, movDst=Reg CX}
                                                                                                        , Binary {binaryOp=op, binaryOperands=(Reg CL, operandr)}
                                                                                                        ]
        fixInstruction Binary {binaryOp=op, binaryOperands=(operandl@(Stack _), operandr@(Stack _))} = [ Mov {movSrc=operandl, movDst=Reg R10}
                                                                                                       , Binary {binaryOp=op, binaryOperands=(Reg R10, operandr)}]
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
translateInstruction (T.Binary { T.binaryOperator=T.Divide, T.binarySrcs=(srcl, srcr), T.binaryDst=dst} : is) =
  [ Mov {movSrc=srcl', movDst=Reg AX}
  , Cdq
  , Idiv srcr'
  , Mov {movSrc=Reg AX, movDst=dst'}
  ] ++ translateInstruction is
  where srcl' = translateOperand srcl
        srcr' = translateOperand srcr
        dst' = translateOperand dst
translateInstruction (T.Binary { T.binaryOperator=T.Remainder, T.binarySrcs=(srcl, srcr), T.binaryDst=dst} : is) =
  [ Mov {movSrc=srcl', movDst=Reg AX}
  , Cdq
  , Idiv srcr'
  , Mov {movSrc=Reg DX, movDst=dst'}
  ] ++ translateInstruction is
  where srcl' = translateOperand srcl
        srcr' = translateOperand srcr
        dst' = translateOperand dst
translateInstruction (T.Binary { T.binaryOperator=op, T.binarySrcs=(srcl, srcr), T.binaryDst=dst} : is) =
  [ Mov {movSrc=srcl', movDst=dst'}
  , Binary {binaryOp=op', binaryOperands=(srcr', dst')}
  ] ++ translateInstruction is
  where srcl' = translateOperand srcl
        srcr' = translateOperand srcr
        dst' = translateOperand dst
        op' = translateBinaryOp op

translateOperand :: T.Val -> Operand
translateOperand (T.Const v) = Imm v
translateOperand (T.Var i)   = Pseudo i

translateUnaryOp :: T.UnaryOperator -> UnaryOperator
translateUnaryOp T.Complement = Not
translateUnaryOp T.Negate     = Neg

translateBinaryOp :: T.BinaryOperator -> BinaryOperator
translateBinaryOp T.Add           = Add
translateBinaryOp T.Multiply      = Mult
translateBinaryOp T.Subtract      = Sub
translateBinaryOp T.Divide        = undefined
translateBinaryOp T.Remainder     = undefined
translateBinaryOp T.BitAnd        = And
translateBinaryOp T.BitOr         = Or
translateBinaryOp T.BitXOR        = Xor
translateBinaryOp T.BitShiftLeft  = ShiftLeft
translateBinaryOp T.BitShiftRight = ShiftRight

