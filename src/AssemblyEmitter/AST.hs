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
  emit (AllocateStack offset) = "subq " `T.append` literal offset `T.append` ", %rsp"

instance Assembly Operand where
  emit :: Operand -> Text
  emit (Reg AX)       = "%eax"
  emit (Reg R10)      = "%r10d"
  emit (Stack offset) = T.pack (show offset) `T.append` "(%rbp)"
  emit (Imm v)        = literal v
  emit _              = ""

instance Assembly UnaryOperator where
  emit :: UnaryOperator -> Text
  emit Neg = "negl"
  emit Not = "notl"

literal :: Int -> Text
literal v = T.pack $ '$' : show v

