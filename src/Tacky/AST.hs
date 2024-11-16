{-# LANGUAGE OverloadedStrings #-}

module Tacky.AST where

import           Data.List  (foldl')
import           Data.Maybe (fromMaybe)
import           Data.Text  (Text)
import qualified Data.Text  as T

import qualified Parser.AST as P

data State = State { stateLastTmp   :: Int
                   , stateLastLabel :: Int
                   }

emptyState :: State
emptyState = State { stateLastTmp = 0
                   , stateLastLabel = 0
                   }

type Identifier = Text

newtype Program = Program FunctionDefinition
  deriving (Show)

data FunctionDefinition = Function { funcIdentifier :: Identifier
                                   , funcBody       :: [Instruction]
                                   }
  deriving (Show)

data Instruction = Return Val
                 | Unary { unaryOperator :: UnaryOperator
                         , unarySrc      :: Val
                         , unaryDst      :: Val
                         }
                 | Binary { binaryOperator :: BinaryOperator
                          , binarySrcs     :: (Val, Val)
                          , binaryDst      :: Val
                          }
                 | Copy { copySrc :: Val
                        , copyDst :: Val
                        }
                 | Jump Identifier
                 | JumpIfZero Val Identifier
                 | JumpIfNotZero Val Identifier
                 | Label Identifier
  deriving (Show)

data UnaryOperator = Complement
                   | Negate
                   | Not
  deriving (Show)

data BinaryOperator = BitOr
                    | BitXOR
                    | BitAnd
                    | EqualsTo
                    | NotEqualsTo
                    | Less
                    | LessOrEqual
                    | Greater
                    | GreaterOrEqual
                    | BitShiftLeft
                    | BitShiftRight
                    | Add
                    | Subtract
                    | Multiply
                    | Divide
                    | Remainder
  deriving (Show)

data Val = Const Int
         | Var Identifier
  deriving (Show)

translateProgram :: State -> P.Program -> (State, Program)
translateProgram st (P.Program func) = Program <$> translateFunction st func

translateFunction :: State -> P.FunctionDefinition -> (State, FunctionDefinition)
translateFunction st P.Function {P.funcName=name, P.funcBody=body} =
  ( st'''
  , Function { funcIdentifier=name
             , funcBody = body'' ++ [ Return $ Const 0 ]
             }
  )
  where (body'', st''') = foldl' (\(body', st') i ->
                                  let (st'', ins) = translateBlockItem st' i
                                  in (body' ++ ins, st''))
                                ([], st)
                                body

translateBlockItem :: State -> P.BlockItem -> (State, [Instruction])
translateBlockItem st (P.Stmt stmt) = translateStatement st stmt
translateBlockItem st (P.Dec  decl) = translateDeclaration st decl

translateStatement :: State -> P.Statement -> (State, [Instruction])
translateStatement st P.Null = (st, [])
translateStatement st (P.Return expr) = (st', expInstructions ++ [Return expVal])
  where (expInstructions, expVal, st') = translateExp st expr
translateStatement st (P.Expression expr) = (st', exprInstructions)
  where (exprInstructions, _, st') = translateExp st expr
translateStatement st (P.If cond stmt1 stmt2) = translateIf st cond stmt1 stmt2

translateDeclaration :: State -> P.Declaration -> (State, [Instruction])
translateDeclaration st (P.Declaration _ Nothing) = (st, [])
translateDeclaration st (P.Declaration lhs (Just rhs)) =
    ( st'
    , rhsInstrucitons
        ++ [ Copy { copySrc = rhs'
                  , copyDst = Var lhs
                  }
           ]
    )
  where (rhsInstrucitons, rhs', st') = translateExp st rhs

translateIf :: State -> P.Exp -> P.Statement -> Maybe P.Statement -> (State, [Instruction])
translateIf st cond thenStmt Nothing =
  (st'''
  , condIns
      ++ [ JumpIfZero cond' endLabel ]
      ++ thenIns
      ++ [ Label endLabel ]
  )
  where (endLabel, st') = newLabel (Just "end") st
        (condIns, cond', st'') = translateExp st' cond
        (st''', thenIns) = translateStatement st'' thenStmt
translateIf st cond thenStmt (Just elseStmt) =
  (st'''''
  , condIns
      ++ [ JumpIfZero cond' elseLabel ]
      ++ thenIns
      ++ [ Jump endLabel
         , Label elseLabel
         ]
      ++ elseIns
      ++ [ Label endLabel ]
  )
  where (endLabel, st')   = newLabel (Just "end") st
        (elseLabel, st'') = newLabel (Just "else") st'
        (condIns, cond', st''') = translateExp st'' cond
        (st'''', thenIns) = translateStatement st''' thenStmt
        (st''''', elseIns) = translateStatement st'''' elseStmt

translateExp :: State -> P.Exp -> ([Instruction], Val, State)
translateExp s (P.Constant (P.CInt val)) = ([], Const val, s)
translateExp s (P.Var var) = ([], Var var, s)
translateExp s (P.Conditional cond expr1 expr2) = translateConditional s cond expr1 expr2

translateExp s (P.Unary  P.Complement      expr)                         = translateUnaryOp        s     Complement     expr
translateExp s (P.Unary  P.Negate          expr)                         = translateUnaryOp        s     Negate         expr
translateExp s (P.Unary  P.Not             expr)                         = translateUnaryOp        s     Not            expr
translateExp s (P.Unary (P.UnaryAssignmentOperator P.PreDecrement)  var) = translatePreAssignment  s var Subtract
translateExp s (P.Unary (P.UnaryAssignmentOperator P.PreIncrement)  var) = translatePreAssignment  s var Add
translateExp s (P.Unary (P.UnaryAssignmentOperator P.PostDecrement) var) = translatePostAssignment s var Subtract
translateExp s (P.Unary (P.UnaryAssignmentOperator P.PostIncrement) var) = translatePostAssignment s var Add

translateExp s (P.Binary P.And             expr1 expr2) = translateAnd      s                expr1 expr2
translateExp s (P.Binary P.Or              expr1 expr2) = translateOr       s                expr1 expr2
translateExp s (P.Binary P.BitOr           expr1 expr2) = translateBinaryOp s BitOr          expr1 expr2
translateExp s (P.Binary P.BitXOR          expr1 expr2) = translateBinaryOp s BitXOR         expr1 expr2
translateExp s (P.Binary P.BitAnd          expr1 expr2) = translateBinaryOp s BitAnd         expr1 expr2
translateExp s (P.Binary P.EqualsTo        expr1 expr2) = translateBinaryOp s EqualsTo       expr1 expr2
translateExp s (P.Binary P.NotEqualsTo     expr1 expr2) = translateBinaryOp s NotEqualsTo    expr1 expr2
translateExp s (P.Binary P.Less            expr1 expr2) = translateBinaryOp s Less           expr1 expr2
translateExp s (P.Binary P.LessOrEqual     expr1 expr2) = translateBinaryOp s LessOrEqual    expr1 expr2
translateExp s (P.Binary P.Greater         expr1 expr2) = translateBinaryOp s Greater        expr1 expr2
translateExp s (P.Binary P.GreaterOrEqual  expr1 expr2) = translateBinaryOp s GreaterOrEqual expr1 expr2
translateExp s (P.Binary P.BitShiftLeft    expr1 expr2) = translateBinaryOp s BitShiftLeft   expr1 expr2
translateExp s (P.Binary P.BitShiftRight   expr1 expr2) = translateBinaryOp s BitShiftRight  expr1 expr2
translateExp s (P.Binary P.Add             expr1 expr2) = translateBinaryOp s Add            expr1 expr2
translateExp s (P.Binary P.Subtract        expr1 expr2) = translateBinaryOp s Subtract       expr1 expr2
translateExp s (P.Binary P.Multiply        expr1 expr2) = translateBinaryOp s Multiply       expr1 expr2
translateExp s (P.Binary P.Divide          expr1 expr2) = translateBinaryOp s Divide         expr1 expr2
translateExp s (P.Binary P.Remainder       expr1 expr2) = translateBinaryOp s Remainder      expr1 expr2

translateExp s (P.Assignment lhs P.AddAssign rhs)           = translateOpAssignment s lhs rhs Add
translateExp s (P.Assignment lhs P.SubAssign rhs)           = translateOpAssignment s lhs rhs Subtract
translateExp s (P.Assignment lhs P.MulAssign rhs)           = translateOpAssignment s lhs rhs Multiply
translateExp s (P.Assignment lhs P.DivAssign rhs)           = translateOpAssignment s lhs rhs Divide
translateExp s (P.Assignment lhs P.RemAssign rhs)           = translateOpAssignment s lhs rhs Remainder
translateExp s (P.Assignment lhs P.BitAndAssign rhs)        = translateOpAssignment s lhs rhs BitAnd
translateExp s (P.Assignment lhs P.BitOrAssign  rhs)        = translateOpAssignment s lhs rhs BitOr
translateExp s (P.Assignment lhs P.BitXORAssign rhs)        = translateOpAssignment s lhs rhs BitXOR
translateExp s (P.Assignment lhs P.BitShiftLeftAssign rhs)  = translateOpAssignment s lhs rhs BitShiftLeft
translateExp s (P.Assignment lhs P.BitShiftRightAssign rhs) = translateOpAssignment s lhs rhs BitShiftRight

translateExp s (P.Assignment lhs P.Assign rhs) =
  (lhsInstructions
    ++ rhsInstrucitons
    ++ [ Copy { copySrc = rhs'
              , copyDst = lhs'
              }
       ]
  , lhs'
  , s'')
  where (lhsInstructions, lhs', s') = translateExp s lhs
        (rhsInstrucitons, rhs', s'') = translateExp s' rhs

translateOpAssignment :: State -> P.Exp -> P.Exp -> BinaryOperator -> ([Instruction], Val, State)
translateOpAssignment s lhs rhs op =
  ( instructions
  , lhs'
  , s''' )
  where (lhsInstructions, lhs', s') = translateExp s lhs
        (rhsInstructions, rhs', s'') = translateExp s' rhs
        (tmp, s''') = newTmpVar s''
        opInstruction = Binary { binaryOperator = op
                               , binarySrcs = (lhs', rhs')
                               , binaryDst = tmp
                               }
        instructions = lhsInstructions
                        ++ rhsInstructions
                        ++ [ opInstruction
                           , Copy { copySrc = tmp
                                  , copyDst = lhs'
                                  }
                           ]

translatePreAssignment :: State -> P.Exp -> BinaryOperator -> ([Instruction], Val, State)
translatePreAssignment s var op =
  ( varInstructions
      ++ [ Binary { binaryOperator = op
                  , binarySrcs = (var', Const 1)
                  , binaryDst = var'}
         ]
  , var'
  , s'
  )
  where (varInstructions, var', s') = translateExp s var

translatePostAssignment :: State -> P.Exp -> BinaryOperator -> ([Instruction], Val, State)
translatePostAssignment s var op =
  ( varInstructions
      ++ [ Copy { copySrc = var'
                , copyDst = tmp
                }
         , Binary { binaryOperator = op
                  , binarySrcs = (var', Const 1)
                  , binaryDst = var'}
         ]
  , tmp
  , s''
  )
  where (varInstructions, var', s') = translateExp s var
        (tmp, s'') = newTmpVar s'

translateBinaryOp :: State -> BinaryOperator -> P.Exp -> P.Exp -> ([Instruction], Val, State)
translateBinaryOp st op expr1 expr2 = (instructions, dst, st''')
  where (exprlInstructions, srcl, st') = translateExp st expr1
        (exprrInstructions, srcr, st'') = translateExp st' expr2
        (dst, st''') = newTmpVar st''
        instruction = Binary {binaryOperator=op, binarySrcs=(srcl, srcr), binaryDst=dst}
        instructions = exprlInstructions ++ exprrInstructions ++ [instruction]

translateUnaryOp :: State -> UnaryOperator -> P.Exp -> ([Instruction], Val, State)
translateUnaryOp s op expr = (instructions, dst, s'')
  where (exprInstructions, src, s') = translateExp s expr
        (dst, s'') = newTmpVar s'
        instruction = Unary {unaryOperator=op, unarySrc=src, unaryDst=dst}
        instructions = exprInstructions ++ [instruction]

translateOr :: State -> P.Exp -> P.Exp -> ([Instruction], Val, State)
translateOr s expr1 expr2 = (instructions, resultVar, s''''')
  where (expr1Instructions, val1, s') = translateExp s expr1
        (expr2Instructions, val2, s'') = translateExp s' expr2
        (trueLabel, s''') = newLabel (Just "OrTrue") s''
        (endLabel, s'''') = newLabel (Just "OrEnd") s'''
        (resultVar, s''''') = newVar (Just "OrResult") s''''
        instructions = expr1Instructions
                    ++ [ JumpIfNotZero val1 trueLabel ]
                    ++ expr2Instructions
                    ++ [ JumpIfNotZero val2 trueLabel
                       , Copy {copySrc=Const 0, copyDst=resultVar}
                       , Jump endLabel
                       , Label trueLabel
                       , Copy {copySrc=Const 1, copyDst=resultVar}
                       , Label endLabel
                       ]

translateAnd :: State -> P.Exp -> P.Exp -> ([Instruction], Val, State)
translateAnd s expr1 expr2 = (instructions, resultVar, s''''')
  where (expr1Instructions, val1, s') = translateExp s expr1
        (expr2Instructions, val2, s'') = translateExp s' expr2
        (falseLabel, s''') = newLabel (Just "AndFalse") s''
        (endLabel, s'''') = newLabel (Just "AndEnd") s'''
        (resultVar, s''''') = newVar (Just "AndResult") s''''
        instructions = expr1Instructions
                    ++ [ JumpIfZero val1 falseLabel ]
                    ++ expr2Instructions
                    ++ [ JumpIfZero val2 falseLabel
                       , Copy {copySrc=Const 1, copyDst=resultVar}
                       , Jump endLabel
                       , Label falseLabel
                       , Copy {copySrc=Const 0, copyDst=resultVar}
                       , Label endLabel
                       ]

translateConditional :: State -> P.Exp -> P.Exp -> P.Exp -> ([Instruction], Val, State)
translateConditional st cond expr1 expr2 =
  ( condIns
      ++ [ JumpIfZero cond' expr2Label ]
      ++ expr1Ins
      ++ copyToResult expr1'
      ++ [ Jump endLabel
         , Label expr2Label
         ]
      ++ expr2Ins
      ++ copyToResult expr2'
      ++ [ Label endLabel ]
  , result
  , st''''''
  )
  where (expr2Label, st') = newLabel (Just "expr2") st
        (endLabel, st'') = newLabel (Just "end") st'
        (result, st''') = newTmpVar st''
        (condIns, cond', st'''') = translateExp st''' cond
        (expr1Ins, expr1', st''''') = translateExp st'''' expr1
        (expr2Ins, expr2', st'''''') = translateExp st''''' expr2
        copyToResult v =  [ Copy { copySrc = v
                                 , copyDst = result
                                 }
                          ]

newVar :: Maybe Text -> State -> (Val, State)
newVar label state@State{ stateLastTmp = lastTmp } = (Var newTmpLabel, state{ stateLastTmp = newTmp })
  where varLabel = T.snoc (fromMaybe "tmp" label) '.'
        newTmp = lastTmp + 1
        newTmpLabel = varLabel <> T.pack (show newTmp)

newTmpVar :: State -> (Val, State)
newTmpVar = newVar Nothing

newLabel :: Maybe Text -> State -> (Identifier, State)
newLabel caption state = (label, state{ stateLastLabel = labelNumber })
  where labelNumber = stateLastLabel state + 1
        labelNumberText = T.pack $ show labelNumber
        label = case caption of Nothing -> labelNumberText
                                Just labelCaption -> labelCaption <> labelNumberText

