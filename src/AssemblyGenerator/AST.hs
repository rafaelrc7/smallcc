module AssemblyGenerator.AST where

import           Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)

import           Numeric.Natural (Natural)
import qualified Tacky.AST       as T

type Identifier = Text

newtype Program = Program [FunctionDefinition]
  deriving (Show)

data FunctionDefinition = Function Identifier [Instruction]
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
                 | DeallocateStack Int
                 | Push Operand
                 | Call Identifier
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
         | CX
         | DX
         | DI
         | SI
         | R8
         | R9
         | R10
         | R11
  deriving (Show)

replacePseudoRegisters :: FunctionDefinition -> (FunctionDefinition, Int)
replacePseudoRegisters function = (function', lastOffsetVarMap varMap''')
  where (function', varMap''') = replaceInFunction newVarMap function

        replaceInFunction :: VarMap -> FunctionDefinition -> (FunctionDefinition, VarMap)
        replaceInFunction varMap (Function name instructions) = (Function name instructions', varMap')
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
        replaceInInstruction varMap (Push op) = (Push op', varMap')
          where (op', varMap') = replaceInOperand varMap op
        replaceInInstruction varMap instruction = (instruction, varMap)

        replaceInOperand :: VarMap -> Operand -> (Operand, VarMap)
        replaceInOperand varMap (Pseudo identifier) = (Stack offset, varMap')
          where (varMap', offset) = fetchVarMap varMap identifier
        replaceInOperand varMap operand = (operand, varMap)

fixInstructions :: (FunctionDefinition, Int) -> FunctionDefinition
fixInstructions (function, lastOffset) = fixFunction function
  where fixFunction :: FunctionDefinition -> FunctionDefinition
        fixFunction (Function name instructions) = Function name instructions'
          where instructions' = AllocateStack (roundToMultipleOf16 lastOffset) : concatMap fixInstruction instructions


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
translateProgram (T.Program functions) = Program $ map (fixInstructions . replacePseudoRegisters . translateFunctionDefinition) functions

translateFunctionDefinition :: T.FunctionDefinition -> FunctionDefinition
translateFunctionDefinition (T.Function name params body) =
  Function name (regParamsIns ++ stackParamsIns ++ translateInstruction body)
  where (regParams', stackParams') = splitAt 6 params
        regParams = zip regParams' [Reg DI, Reg SI, Reg DX, Reg CX, Reg R8, Reg R9]
        stackParams = zip stackParams' [0..]
        regParamsIns = translateRegisterParameters regParams
        stackParamsIns = translateStackParameters stackParams

translateRegisterParameters :: [(Identifier, Operand)] -> [Instruction]
translateRegisterParameters []                  = []
translateRegisterParameters ((param, reg):params) = Mov { movSrc = reg, movDst = Pseudo param } : translateRegisterParameters params

translateStackParameters :: [(Identifier, Natural)] -> [Instruction]
translateStackParameters []                     = []
translateStackParameters ((param, paramN):params) = Mov { movSrc = Stack stackOffset, movDst = Pseudo param } : translateStackParameters params
  where stackOffset = fromInteger $ toInteger $ 16 + 8*paramN


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
translateInstruction (T.Unary { T.unaryOperator=T.Complement, T.unarySrc=src, T.unaryDst=dst} : is) =
  translateUnaryInstruction Not src dst ++ translateInstruction is
translateInstruction (T.Unary { T.unaryOperator=T.Negate, T.unarySrc=src, T.unaryDst=dst} : is) =
  translateUnaryInstruction Neg src dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=T.EqualsTo, T.binarySrcs=(src1, src2), T.binaryDst=dst} : is) =
  translateCmpInstruction E src1 src2 dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=T.NotEqualsTo, T.binarySrcs=(src1, src2), T.binaryDst=dst} : is) =
  translateCmpInstruction NE src1 src2 dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=T.Less, T.binarySrcs=(src1, src2), T.binaryDst=dst} : is) =
  translateCmpInstruction L src1 src2 dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=T.LessOrEqual, T.binarySrcs=(src1, src2), T.binaryDst=dst} : is) =
  translateCmpInstruction LE src1 src2 dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=T.Greater, T.binarySrcs=(src1, src2), T.binaryDst=dst} : is) =
  translateCmpInstruction G src1 src2 dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=T.GreaterOrEqual, T.binarySrcs=(src1, src2), T.binaryDst=dst} : is) =
  translateCmpInstruction GE src1 src2 dst ++ translateInstruction is
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
translateInstruction (T.Binary { T.binaryOperator=T.BitOr, T.binarySrcs=(srcl, srcr), T.binaryDst=dst} : is) =
  translateBinaryInstruction Or srcl srcr dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=T.BitXOR, T.binarySrcs=(srcl, srcr), T.binaryDst=dst} : is) =
  translateBinaryInstruction Xor srcl srcr dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=T.BitAnd, T.binarySrcs=(srcl, srcr), T.binaryDst=dst} : is) =
  translateBinaryInstruction And srcl srcr dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=T.BitShiftLeft, T.binarySrcs=(srcl, srcr), T.binaryDst=dst} : is) =
  translateBinaryInstruction ShiftLeft srcl srcr dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=T.BitShiftRight, T.binarySrcs=(srcl, srcr), T.binaryDst=dst} : is) =
  translateBinaryInstruction ShiftRight srcl srcr dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=T.Add, T.binarySrcs=(srcl, srcr), T.binaryDst=dst} : is) =
  translateBinaryInstruction Add srcl srcr dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=T.Subtract, T.binarySrcs=(srcl, srcr), T.binaryDst=dst} : is) =
  translateBinaryInstruction Sub srcl srcr dst ++ translateInstruction is
translateInstruction (T.Binary { T.binaryOperator=T.Multiply, T.binarySrcs=(srcl, srcr), T.binaryDst=dst} : is) =
  translateBinaryInstruction Mult srcl srcr dst ++ translateInstruction is
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
translateInstruction (T.FunCall name args ret : is) =
    stackPaddingIns
      ++ translateRegisterArguments regArgs
      ++ translateStackArguments stackArgs
      ++ [ Call name ]
      ++ stackCleanupIns
      ++ [ Mov { movSrc = Reg AX, movDst = ret' } ]
      ++ translateInstruction is
  where (regArgs', stackArgs') = splitAt 6 args
        regArgs = zip regArgs' [Reg DI, Reg SI, Reg DX, Reg CX, Reg R8, Reg R9]
        stackArgs = reverse stackArgs'
        stackArgsLen = length stackArgs
        stackPadding = if odd stackArgsLen then 8 else 0
        stackPaddingIns = [AllocateStack stackPadding | stackPadding > 0]
        bytesToRemove = 8 * stackArgsLen + stackPadding
        stackCleanupIns = [DeallocateStack bytesToRemove | bytesToRemove > 0]
        ret' = translateOperand ret

translateUnaryInstruction :: UnaryOperator -> T.Val -> T.Val -> [Instruction]
translateUnaryInstruction op src dst =
  [ Mov {movSrc=src', movDst=dst'}
  , Unary {unaryOp=op, unaryOperand=dst'}
  ]
  where src' = translateOperand src
        dst' = translateOperand dst

translateBinaryInstruction :: BinaryOperator -> T.Val -> T.Val -> T.Val -> [Instruction]
translateBinaryInstruction op srcl srcr dst =
  [ Mov {movSrc=srcl', movDst=dst'}
  , Binary {binaryOp=op, binaryOperands=(srcr', dst')}
  ]
  where srcl' = translateOperand srcl
        srcr' = translateOperand srcr
        dst' = translateOperand dst

translateCmpInstruction :: Conditional -> T.Val -> T.Val -> T.Val -> [Instruction]
translateCmpInstruction op src1 src2 dst =
  [ Cmp src2' src1'
  , Mov (Imm 0) dst'
  , SetCC op dst'
  ]
  where src1' = translateOperand src1
        src2' = translateOperand src2
        dst' = translateOperand dst

translateOperand :: T.Val -> Operand
translateOperand (T.Const v) = Imm v
translateOperand (T.Var i)   = Pseudo i

translateRegisterArguments :: [(T.Val, Operand)] -> [Instruction]
translateRegisterArguments []                = []
translateRegisterArguments ((val, reg):args) = Mov { movSrc=val', movDst=reg } : translateRegisterArguments args
  where val' = translateOperand val

translateStackArguments :: [T.Val] -> [Instruction]
translateStackArguments []         = []
translateStackArguments (arg:args) = ins ++ translateStackArguments args
  where val = translateOperand arg
        ins = case val of
                v@(Imm _) -> [ Push v ]
                v@(Reg _) -> [ Push v ]
                v         -> [ Mov { movSrc = v
                                   , movDst = Reg AX
                                   }
                             , Push (Reg AX)
                             ]

roundToMultiple :: Int -> Int -> Int
roundToMultiple 0 num = num
roundToMultiple multiple num = if remainder == 0 then num
                                                 else num + multiple - remainder
  where remainder = num `rem` multiple

roundToMultipleOf16 :: Int -> Int
roundToMultipleOf16 = roundToMultiple 16
