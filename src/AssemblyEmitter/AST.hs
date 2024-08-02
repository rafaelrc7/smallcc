{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module AssemblyEmitter.AST where

import           Data.Text             (Text)
import qualified Data.Text             as T

import           AssemblyGenerator.AST

class Assembly a where
  emit :: a -> Text

instance Assembly Program where
  emit :: Program -> Text
  emit (Program func) = emit func `T.append` ".section .note.GNU-stack,\"\",@progbits\n\n"

instance Assembly FunctionDefinition where
  emit :: FunctionDefinition -> Text
  emit func = ".globl " `T.append` name `T.append` "\n"
                        `T.append` name `T.append` ":\n"
                        `T.append` "\tpushq %rbp\n"
                        `T.append` "\tmovq %rsp, %rbp\n"
                        `T.append` instructions
    where name = funcName func
          instructions = flip T.snoc '\n' $ T.concat $ map (T.unlines . map (T.cons '\t') . T.lines . emit) $ funcInstructions func

instance Assembly Instruction where
  emit :: Instruction -> Text
  emit Mov {movSrc=src, movDst=dst} = "movl " `T.append` emit src `T.append` ", " `T.append` emit dst
  emit Ret = "movq %rbp, %rsp\
             \\npopq %rbp\
             \\nret"
  emit Unary {unaryOperand=operand, unaryOp=op} = T.intercalate " " [emit op, emit operand]
  emit Binary {binaryOperands=(Reg reg, op2), binaryOp=op@ShiftLeft} = emit op `T.append` " " `T.append` emitReg B1 reg `T.append` ", " `T.append` emit op2
  emit Binary {binaryOperands=(Reg reg, op2), binaryOp=op@ShiftRight} = emit op `T.append` " " `T.append` emitReg B1 reg `T.append` ", " `T.append` emit op2
  emit Binary {binaryOperands=(op1, op2), binaryOp=op} = emit op `T.append` " " `T.append` emit op1 `T.append` ", " `T.append` emit op2
  emit (Idiv operand) = "idivl " `T.append` emit operand
  emit Cdq = "cdq"
  emit (AllocateStack offset) = "subq " `T.append` literal offset `T.append` ", %rsp"
  emit (Cmp op1 op2) = "cmpl " `T.append` emit op1 `T.append` ", " `T.append` emit op2
  emit (Jmp label) = "jmp .L" `T.append` label
  emit (JmpCC cond label) = "j" `T.append` emit cond `T.append` " .L" `T.append` label
  emit (SetCC cond (Reg reg)) = "set" `T.append` emit cond `T.append` " " `T.append` emitReg B1 reg
  emit (SetCC cond operand) = "set" `T.append` emit cond `T.append` " " `T.append` emit operand
  emit (Label label) = ".L" `T.append` label `T.append` ":"

instance Assembly Operand where
  emit :: Operand -> Text
  emit (Reg reg)      = emit reg
  emit (Stack offset) = T.pack (show offset) `T.append` "(%rbp)"
  emit (Imm v)        = literal v
  emit _              = ""

instance Assembly Conditional where
  emit :: Conditional -> Text
  emit E  = "e"
  emit NE = "ne"
  emit G  = "g"
  emit GE = "ge"
  emit L  = "l"
  emit LE = "le"

data Size = B1
          | B4

instance Assembly Reg where
  emit :: Reg -> Text
  emit = emitReg B4

emitReg :: Size -> Reg -> Text
emitReg B1 AX  = "%al"
emitReg B1 DX  = "%dl"
emitReg B1 CX  = "%cl"
emitReg B1 R10 = "%r10b"
emitReg B1 R11 = "%r11b"

emitReg B4 AX  = "%eax"
emitReg B4 DX  = "%edx"
emitReg B4 CX  = "%ecx"
emitReg B4 R10 = "%r10d"
emitReg B4 R11 = "%r11d"

instance Assembly UnaryOperator where
  emit :: UnaryOperator -> Text
  emit Neg = "negl"
  emit Not = "notl"

instance Assembly BinaryOperator where
  emit :: BinaryOperator -> Text
  emit Add        = "addl"
  emit Sub        = "subl"
  emit Mult       = "imull"
  emit And        = "andl"
  emit Or         = "orl"
  emit Xor        = "xorl"
  emit ShiftLeft  = "sall"
  emit ShiftRight = "sarl"

literal :: Int -> Text
literal v = T.pack $ '$' : show v

