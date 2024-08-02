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
                 | Cmp Operand Operand
                 | Idiv Operand
                 | Cdq
                 | Jmp Identifier
                 | JmpCC Conditional Identifier
                 | SetCC Conditional Operand
                 | Label Identifier
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

data Conditional = E
                 | NE
                 | G
                 | GE
                 | L
                 | LE
  deriving (Show)

data Reg = AX
         | DX
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
        replaceInInstruction varMap (Cmp op1 op2) = (Cmp op1' op2', varMap'')
          where (op1', varMap') = replaceInOperand varMap op1
                (op2', varMap'') = replaceInOperand varMap' op2
        replaceInInstruction varMap (SetCC cond op) = (SetCC cond op', varMap')
          where (op', varMap') = replaceInOperand varMap op
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
                                                                                                       , Binary {binaryOp=op, binaryOperands=(Reg CX, operandr)}
                                                                                                       ]
        fixInstruction Binary {binaryOp=op@ShiftRight, binaryOperands=(operandl@(Stack _), operandr)} = [ Mov {movSrc=operandl, movDst=Reg CX}
                                                                                                        , Binary {binaryOp=op, binaryOperands=(Reg CX, operandr)}
                                                                                                        ]
        fixInstruction Binary {binaryOp=op, binaryOperands=(operandl@(Stack _), operandr@(Stack _))} = [ Mov {movSrc=operandl, movDst=Reg R10}
                                                                                                       , Binary {binaryOp=op, binaryOperands=(Reg R10, operandr)}]
        fixInstruction (Cmp op1@(Stack _) op2@(Stack _)) = [ Mov {movSrc=op1, movDst=Reg R10}
                                                           , Cmp (Reg R10) op2
                                                           ]
        fixInstruction (Cmp op1 op2@(Imm _)) = [ Mov {movSrc=op2, movDst=Reg R11}
                                               , Cmp op1 (Reg R11)
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
translateInstruction (T.Unary { T.unaryOperator=T.Not, T.unarySrc=src, T.unaryDst=dst} : is) =
  [ Cmp (Imm 0) src'
  , Mov (Imm 0) dst'
  , SetCC E dst'
  ] ++ translateInstruction is
  where src' = translateOperand src
        dst' = translateOperand dst
translateInstruction (T.Unary { T.unaryOperator=op, T.unarySrc=src, T.unaryDst=dst} : is) =
  [ Mov {movSrc=src', movDst=dst'}
  , Unary {unaryOp=op', unaryOperand=dst'}
  ] ++ translateInstruction is
  where src' = translateOperand src
        dst' = translateOperand dst
        op' = translateUnaryOp op
translateInstruction (T.Binary { T.binaryOperator=op@T.Equals, T.binarySrcs=(src1, src2), T.binaryDst=dst} : is) =
  translateCmpInstruction op src1 src2 dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=op@T.NotEquals, T.binarySrcs=(src1, src2), T.binaryDst=dst} : is) =
  translateCmpInstruction op src1 src2 dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=op@T.Less, T.binarySrcs=(src1, src2), T.binaryDst=dst} : is) =
  translateCmpInstruction op src1 src2 dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=op@T.LessOrEqual, T.binarySrcs=(src1, src2), T.binaryDst=dst} : is) =
  translateCmpInstruction op src1 src2 dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=op@T.Greater, T.binarySrcs=(src1, src2), T.binaryDst=dst} : is) =
  translateCmpInstruction op src1 src2 dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=op@T.GreaterOrEqual, T.binarySrcs=(src1, src2), T.binaryDst=dst} : is) =
  translateCmpInstruction op src1 src2 dst ++ translateInstruction is
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
translateInstruction (T.Jump label : is) =
  Jmp label : translateInstruction is
translateInstruction (T.JumpIfZero val label : is) =
  [ Cmp (Imm 0) (translateOperand val)
  , JmpCC E label
  ] ++ translateInstruction is
translateInstruction (T.JumpIfNotZero val label : is) =
  [ Cmp (Imm 0) (translateOperand val)
  , JmpCC NE label
  ] ++ translateInstruction is
translateInstruction (T.Copy src dst : is) =
  Mov (translateOperand src) (translateOperand dst) : translateInstruction is
translateInstruction (T.Label label : is) =
  Label label : translateInstruction is

translateCmpInstruction :: T.BinaryOperator -> T.Val -> T.Val -> T.Val -> [Instruction]
translateCmpInstruction op src1 src2 dst =
  [ Cmp src2' src1'
  , Mov (Imm 0) dst'
  , SetCC cond dst'
  ]
  where src1' = translateOperand src1
        src2' = translateOperand src2
        dst' = translateOperand dst
        cond = translateCmpOp op

translateCmpOp :: T.BinaryOperator -> Conditional
translateCmpOp T.Equals         = E
translateCmpOp T.NotEquals      = NE
translateCmpOp T.Less           = L
translateCmpOp T.LessOrEqual    = LE
translateCmpOp T.Greater        = G
translateCmpOp T.GreaterOrEqual = GE
translateCmpOp _                = undefined

translateOperand :: T.Val -> Operand
translateOperand (T.Const v) = Imm v
translateOperand (T.Var i)   = Pseudo i

translateUnaryOp :: T.UnaryOperator -> UnaryOperator
translateUnaryOp T.Complement = Not
translateUnaryOp T.Negate     = Neg
translateUnaryOp T.Not        = undefined

translateBinaryOp :: T.BinaryOperator -> BinaryOperator
translateBinaryOp T.BitOr          = Or
translateBinaryOp T.BitXOR         = Xor
translateBinaryOp T.BitAnd         = And
translateBinaryOp T.Equals         = undefined
translateBinaryOp T.NotEquals      = undefined
translateBinaryOp T.Less           = undefined
translateBinaryOp T.LessOrEqual    = undefined
translateBinaryOp T.Greater        = undefined
translateBinaryOp T.GreaterOrEqual = undefined
translateBinaryOp T.BitShiftLeft   = ShiftLeft
translateBinaryOp T.BitShiftRight  = ShiftRight
translateBinaryOp T.Add            = Add
translateBinaryOp T.Subtract       = Sub
translateBinaryOp T.Multiply       = Mult
translateBinaryOp T.Divide         = undefined
translateBinaryOp T.Remainder      = undefined

