{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module AssemblyEmitter.AST where

import           AssemblyGenerator.AST
import           Control.Monad.Reader  (MonadReader (ask, local), ReaderT)
import           Control.Monad.Writer  (Writer, tell)
import           Data.Set              (Set)
import qualified Data.Set              as S
import           Data.Text             (Text)
import qualified Data.Text             as T

data Size
  = B1
  | B4
  | B8

type AssemblyEmissionMonad a = ReaderT (Set Identifier) (Writer Text) a

class AssemblyEmitter a where
  emit :: a -> AssemblyEmissionMonad ()

instance AssemblyEmitter Program where
  emit :: Program -> AssemblyEmissionMonad ()
  emit (Program functions) =
    local (const functionSet) $ mapM_ (\f -> emit f >> tell "\n\n") functions >> tell ".section .note.GNU-stack,\"\",@progbits\n\n"
    where
      functionSet = S.fromList $ map (\(Function name _) -> name) functions

instance AssemblyEmitter FunctionDefinition where
  emit :: FunctionDefinition -> AssemblyEmissionMonad ()
  emit (Function name instructions) =
    do
      tell $
        ".globl "
          <> name
          <> "\n"
          <> name
          <> ":\n"
          <> "pushq %rbp\n"
          <> "movq %rsp, %rbp\n"
      mapM_ (\i -> emit i >> tell "\n") instructions

instance AssemblyEmitter Instruction where
  emit :: Instruction -> AssemblyEmissionMonad ()
  emit Mov {movSrc = src, movDst = dst} = tell "movl " >> emit src >> tell ", " >> emit dst
  emit Ret =
    tell
      "movq %rbp, %rsp\n\
      \popq %rbp\n\
      \ret"
  emit Unary {unaryOperand = operand, unaryOp = op} = emit op >> tell " " >> emit operand
  emit Binary {binaryOperands = (Reg reg, op2), binaryOp = op@ShiftLeft} = emit op >> tell " " >> tell (emitReg B1 reg) >> tell ", " >> emit op2
  emit Binary {binaryOperands = (Reg reg, op2), binaryOp = op@ShiftRight} = emit op >> tell " " >> tell (emitReg B1 reg) >> tell ", " >> emit op2
  emit Binary {binaryOperands = (op1, op2), binaryOp = op} = emit op >> tell " " >> emit op1 >> tell ", " >> emit op2
  emit (Idiv operand) = tell "idivl " >> emit operand
  emit Cdq = tell "cdq"
  emit (AllocateStack offset) = tell $ "subq " <> literal offset <> ", %rsp"
  emit (DeallocateStack offset) = tell $ "addq " <> literal offset <> ", %rsp"
  emit (Cmp op1 op2) = tell "cmpl " >> emit op1 >> tell ", " >> emit op2
  emit (Jmp label) = tell $ "jmp .L" <> label
  emit (JmpCC cond label) = tell "j" >> emit cond >> tell " .L" >> tell label
  emit (SetCC cond (Reg reg)) = tell "set" >> emit cond >> tell " " >> tell (emitReg B1 reg)
  emit (SetCC cond operand) = tell "set" >> emit cond >> tell " " >> emit operand
  emit (Label label) = tell $ ".L" <> label <> ":"
  emit (Push (Reg reg)) = tell $ "pushq " <> emitReg B8 reg
  emit (Push operand) = tell "pushq " >> emit operand
  emit (Call label) =
    do
      functionsSet <- ask
      let suffix = if S.member label functionsSet then "" else "@PLT"
      tell $ "call " <> label <> suffix

instance AssemblyEmitter Operand where
  emit :: Operand -> AssemblyEmissionMonad ()
  emit (Reg reg)      = emit reg
  emit (Stack offset) = tell $ T.pack (show offset) <> "(%rbp)"
  emit (Imm v)        = tell $ literal v
  emit (Pseudo _)     = pure ()

instance AssemblyEmitter Conditional where
  emit :: Conditional -> AssemblyEmissionMonad ()
  emit E  = tell "e"
  emit NE = tell "ne"
  emit G  = tell "g"
  emit GE = tell "ge"
  emit L  = tell "l"
  emit LE = tell "le"

instance AssemblyEmitter Reg where
  emit :: Reg -> AssemblyEmissionMonad ()
  emit = tell . emitReg B4

instance AssemblyEmitter UnaryOperator where
  emit :: UnaryOperator -> AssemblyEmissionMonad ()
  emit Neg = tell "negl"
  emit Not = tell "notl"

instance AssemblyEmitter BinaryOperator where
  emit :: BinaryOperator -> AssemblyEmissionMonad ()
  emit Add        = tell "addl"
  emit Sub        = tell "subl"
  emit Mult       = tell "imull"
  emit And        = tell "andl"
  emit Or         = tell "orl"
  emit Xor        = tell "xorl"
  emit ShiftLeft  = tell "sall"
  emit ShiftRight = tell "sarl"

emitReg :: Size -> Reg -> Text
emitReg B1 AX  = "%al"
emitReg B1 CX  = "%cl"
emitReg B1 DX  = "%dl"
emitReg B1 DI  = "%dil"
emitReg B1 SI  = "%sil"
emitReg B1 R8  = "%r8b"
emitReg B1 R9  = "%r9b"
emitReg B1 R10 = "%r10b"
emitReg B1 R11 = "%r11b"
emitReg B4 AX  = "%eax"
emitReg B4 CX  = "%ecx"
emitReg B4 DX  = "%edx"
emitReg B4 DI  = "%edi"
emitReg B4 SI  = "%esi"
emitReg B4 R8  = "%r8d"
emitReg B4 R9  = "%r9d"
emitReg B4 R10 = "%r10d"
emitReg B4 R11 = "%r11d"
emitReg B8 AX  = "%rax"
emitReg B8 CX  = "%rcx"
emitReg B8 DX  = "%rdx"
emitReg B8 DI  = "%rdi"
emitReg B8 SI  = "%rsi"
emitReg B8 R8  = "%r8"
emitReg B8 R9  = "%r9"
emitReg B8 R10 = "%r10"
emitReg B8 R11 = "%r11"

literal :: Int -> Text
literal v = T.pack $ '$' : show v
