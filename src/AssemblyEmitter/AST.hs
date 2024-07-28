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
                        `T.append` instructions
    where name = funcName func
          instructions = flip T.snoc '\n' $ T.concat $ map (flip T.snoc '\n' . T.cons '\t' . emit) $ funcInstructions func

instance Assembly Instruction where
  emit :: Instruction -> Text
  emit Ret = "ret"
  emit Mov {movSrc=src, movDst=dst} = "movl " `T.append` emit src `T.append` ", " `T.append` emit dst

instance Assembly Operand where
  emit :: Operand -> Text
  emit (Imm v)  = T.pack $ '$' : show v
  emit Register = "%eax"

