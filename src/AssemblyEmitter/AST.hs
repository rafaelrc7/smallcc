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
  emit Binary {binaryOperands=(src, dst), binaryOp=op} = emit op `T.append` " " `T.append` emit src `T.append` ", " `T.append` emit dst
  emit (Idiv operand) = "idivl " `T.append` emit operand
  emit Cdq = "cdq"
  emit (AllocateStack offset) = "subq " `T.append` literal offset `T.append` ", %rsp"

instance Assembly Operand where
  emit :: Operand -> Text
  emit (Reg reg)      = emit reg
  emit (Stack offset) = T.pack (show offset) `T.append` "(%rbp)"
  emit (Imm v)        = literal v
  emit _              = ""

instance Assembly Reg where
  emit :: Reg -> Text
  emit AX  = "%eax"
  emit DX  = "%edx"
  emit R10 = "%r10d"
  emit R11 = "%r11d"

instance Assembly UnaryOperator where
  emit :: UnaryOperator -> Text
  emit Neg = "negl"
  emit Not = "notl"

instance Assembly BinaryOperator where
  emit :: BinaryOperator -> Text
  emit Add  = "addl"
  emit Sub  = "subl"
  emit Mult = "imull"

literal :: Int -> Text
literal v = T.pack $ '$' : show v

