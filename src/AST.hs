{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module AST where

import           Data.Text       (Text)
import qualified Data.Text       as T
import           GHC.Types       (Constraint, Type)
import           Numeric.Natural (Natural)
import           Pretty          (PrettyPrinter (..), identLines)

type Identifier = Text
type Label = Identifier

data Pass = Parsed | LoopLabeled
  deriving (Show)

-- AST Definition --

data Program p where
  Program :: FunctionDefinition p -> Program p
deriving instance (Forall Show (p :: Pass)) => Show (Program p)

data FunctionDefinition p where
  Function :: Identifier -> Block p -> FunctionDefinition p
deriving instance (Forall Show (p :: Pass)) => Show (FunctionDefinition p)

data BlockItem p where
  Stmt :: Statement p -> BlockItem p
  Dec :: Declaration -> BlockItem p
deriving instance (Forall Show (p :: Pass)) => Show (BlockItem p)

data Block p where
  Block :: [BlockItem p] -> Block p
deriving instance (Forall Show (p :: Pass)) => Show (Block p)

data Statement (p :: Pass) where
  Return :: (XReturn p) -> Exp -> Statement p
  Expression :: (XExpression p) -> Exp -> Statement p
  If :: (XIf p) -> Exp -> Statement p -> Maybe (Statement p) -> Statement p
  Compound :: (XCompound p) -> Block p -> Statement p
  Goto :: (XGoto p) -> Identifier -> Statement p
  Label :: (XLabel p) -> Identifier -> Statement p
  Break :: (XBreak p) -> Statement p
  Continue :: (XContinue p) -> Statement p
  While :: (XWhile p) -> Exp -> Statement p -> Statement p
  DoWhile :: (XDoWhile p) -> Statement p -> Exp -> Statement p
  For :: (XFor p) -> ForInit -> Maybe Exp -> Maybe Exp -> Statement p -> Statement p
  Null :: (XNull p) -> Statement p
deriving instance (Forall Show (p :: Pass)) => Show (Statement p)

data ForInit where
  InitDecl :: Declaration -> ForInit
  InitExp :: Maybe Exp -> ForInit
deriving instance Show ForInit

data Declaration where
  Declaration :: Identifier -> Maybe Exp -> Declaration
deriving instance Show Declaration

data Exp where
  Constant :: Constant -> Exp
  Var :: Identifier -> Exp
  Unary :: UnaryOperator -> Exp -> Exp
  Binary :: Exp -> BinaryOperator -> Exp -> Exp
  Assignment :: Exp -> AssignmentOperator -> Exp -> Exp
  Conditional :: Exp -> Exp -> Exp -> Exp
deriving instance Show Exp

newtype Constant = CInt Int
  deriving (Show)

data UnaryOperator = Complement
                   | Negate
                   | Not
                   | UnaryAssignmentOperator UnaryAssignmentOperator
  deriving (Show)

data UnaryAssignmentOperator = PreDecrement
                             | PreIncrement
                             | PostDecrement
                             | PostIncrement
  deriving (Show)

data BinaryOperator = Add
                    | Subtract
                    | Multiply
                    | Divide
                    | Remainder
                    | BitAnd
                    | BitOr
                    | BitXOR
                    | BitShiftLeft
                    | BitShiftRight
                    | And
                    | Or
                    | EqualsTo
                    | NotEqualsTo
                    | Less
                    | LessOrEqual
                    | Greater
                    | GreaterOrEqual
  deriving (Show)

data AssignmentOperator = Assign
                        | AddAssign
                        | SubAssign
                        | MulAssign
                        | DivAssign
                        | RemAssign
                        | BitAndAssign
                        | BitOrAssign
                        | BitXORAssign
                        | BitShiftLeftAssign
                        | BitShiftRightAssign
  deriving (Show)

-- Decorators --

type Forall (c :: Type -> Constraint) p = (c (XReturn p), c (XExpression p), c (XIf p), c (XCompound p), c (XGoto p), c (XLabel p), c (XBreak p), c (XContinue p), c (XWhile p), c (XDoWhile p), c (XFor p), c (XNull p))

type family XReturn     (p :: Pass)
type family XExpression (p :: Pass)
type family XIf         (p :: Pass)
type family XCompound   (p :: Pass)
type family XGoto       (p :: Pass)
type family XLabel      (p :: Pass)
type family XBreak      (p :: Pass)
type family XContinue   (p :: Pass)
type family XWhile      (p :: Pass)
type family XDoWhile    (p :: Pass)
type family XFor        (p :: Pass)
type family XNull       (p :: Pass)

type instance XReturn     Parsed = ()
type instance XExpression Parsed = ()
type instance XIf         Parsed = ()
type instance XCompound   Parsed = ()
type instance XGoto       Parsed = ()
type instance XLabel      Parsed = ()
type instance XBreak      Parsed = ()
type instance XContinue   Parsed = ()
type instance XWhile      Parsed = ()
type instance XDoWhile    Parsed = ()
type instance XFor        Parsed = ()
type instance XNull       Parsed = ()

type instance XReturn     LoopLabeled = ()
type instance XExpression LoopLabeled = ()
type instance XIf         LoopLabeled = ()
type instance XCompound   LoopLabeled = ()
type instance XGoto       LoopLabeled = Identifier
type instance XLabel      LoopLabeled = Identifier
type instance XBreak      LoopLabeled = Identifier
type instance XContinue   LoopLabeled = Identifier
type instance XWhile      LoopLabeled = Identifier
type instance XDoWhile    LoopLabeled = Identifier
type instance XFor        LoopLabeled = Identifier
type instance XNull       LoopLabeled = ()

data Associativity = LeftAssociative | RightAssociative

data Precedence = Precedence Associativity Natural

-- https://en.cppreference.com/w/c/language/operator_precedence
precedence :: BinaryOperator -> Precedence
precedence Or             = Precedence LeftAssociative  5
precedence And            = Precedence LeftAssociative  10
precedence BitOr          = Precedence LeftAssociative  15
precedence BitXOR         = Precedence LeftAssociative  20
precedence BitAnd         = Precedence LeftAssociative  25
precedence EqualsTo       = Precedence LeftAssociative  30
precedence NotEqualsTo    = Precedence LeftAssociative  30
precedence Less           = Precedence LeftAssociative  35
precedence LessOrEqual    = Precedence LeftAssociative  35
precedence Greater        = Precedence LeftAssociative  35
precedence GreaterOrEqual = Precedence LeftAssociative  35
precedence BitShiftLeft   = Precedence LeftAssociative  40
precedence BitShiftRight  = Precedence LeftAssociative  40
precedence Add            = Precedence LeftAssociative  45
precedence Subtract       = Precedence LeftAssociative  45
precedence Multiply       = Precedence LeftAssociative  50
precedence Divide         = Precedence LeftAssociative  50
precedence Remainder      = Precedence LeftAssociative  50

instance PrettyPrinter (Program p) where
  pretty :: Program p -> Text
  pretty (Program f) = "Program\n" <>  f'
    where f' = identLines $ pretty f

instance PrettyPrinter (FunctionDefinition p) where
  pretty :: FunctionDefinition p -> Text
  pretty (Function name body) = "Function '" <> name <> "'\n" <> pretty body <> "\n"

instance PrettyPrinter (Block p) where
  pretty :: Block p -> Text
  pretty (Block blockItens) = T.concat $ map (identLines . pretty) blockItens

instance PrettyPrinter (BlockItem p) where
  pretty :: BlockItem p -> Text
  pretty (Stmt stmt) = pretty stmt
  pretty (Dec dec)   = pretty dec

instance PrettyPrinter (Statement p) where
  pretty :: Statement p -> Text
  pretty (Return _ expr) = ret <> expr' <> ";\n"
    where ret = "return "
          expr' = pretty expr
  pretty (Expression _ expr) = pretty expr <> ";\n"
  pretty (If _ cond expThen Nothing) = "if (" <> pretty cond <> ")\n" <> identLines (pretty expThen)
  pretty (If _ cond expThen (Just expElse)) = "if (" <> pretty cond <> ")\n" <> identLines (pretty expThen) <> "\nelse\n" <> identLines (pretty expElse)
  pretty (Compound _ block) = "{\n" <> pretty block <> "\n}\n"
  pretty (Goto _ label) = "goto " <> pretty label <> ";\n"
  pretty (Label _ label) = pretty label <> ":\n"
  pretty (Break _) = "break;\n"
  pretty (Continue _) = "continue;\n"
  pretty (While _ cond body) = "while (" <> pretty cond <> ")" <> pretty body
  pretty (DoWhile _ body cond) = "do" <> pretty body <> "while (" <> pretty cond <> ");\n"
  pretty (For _ ini cond post body) = "for (" <> pretty ini <> "; " <> maybe "" pretty cond <> "; " <> maybe "" pretty post <> ")\n" <> pretty body
  pretty (Null _) = ";\n"

instance PrettyPrinter ForInit where
  pretty :: ForInit -> Text
  pretty (InitDecl decl) = pretty decl
  pretty (InitExp  expr) = maybe "" pretty expr

instance PrettyPrinter Declaration where
  pretty :: Declaration -> Text
  pretty (Declaration name Nothing) = "int " <> name <> ";\n";
  pretty (Declaration name (Just i)) = "int " <> name <> " = " <> pretty i <> ";\n";

instance PrettyPrinter Exp where
  pretty :: Exp -> Text
  pretty (Constant val) = pretty val
  pretty (Var var) = var
  pretty (Unary op@(UnaryAssignmentOperator PostDecrement) expr) = "(" <> pretty expr <> pretty op <> ")"
  pretty (Unary op@(UnaryAssignmentOperator PostIncrement) expr) = "(" <> pretty expr <> pretty op <> ")"
  pretty (Unary op expr) = "(" <> pretty op <> pretty expr <> ")"
  pretty (Binary op exprl exprr) = "(" <> T.intercalate " " [pretty exprl, pretty op, pretty exprr] <> ")"
  pretty (Assignment exprl op exprr) = "(" <> T.intercalate " " [pretty exprl, pretty op, pretty exprr] <> ")"
  pretty (Conditional cond exp1 exp2) = "(" <> pretty cond <> " ? " <> pretty exp1 <> " : " <> pretty exp2 <> ")"

instance PrettyPrinter Constant where
  pretty :: Constant -> Text
  pretty (CInt val) = T.pack $ show val

instance PrettyPrinter UnaryOperator where
  pretty :: UnaryOperator -> Text
  pretty Complement                   = "~"
  pretty Negate                       = "-"
  pretty Not                          = "!"
  pretty (UnaryAssignmentOperator op) = pretty op

instance PrettyPrinter UnaryAssignmentOperator where
  pretty :: UnaryAssignmentOperator -> Text
  pretty PreDecrement  = "--"
  pretty PreIncrement  = "++"
  pretty PostDecrement = "--"
  pretty PostIncrement = "++"

instance PrettyPrinter BinaryOperator where
  pretty :: BinaryOperator -> Text
  pretty Or             = "||"
  pretty And            = "&&"
  pretty BitOr          = "|"
  pretty BitXOR         = "^"
  pretty BitAnd         = "&"
  pretty EqualsTo       = "=="
  pretty NotEqualsTo    = "!="
  pretty Less           = "<"
  pretty LessOrEqual    = "<="
  pretty Greater        = ">"
  pretty GreaterOrEqual = ">="
  pretty BitShiftLeft   = "<<"
  pretty BitShiftRight  = ">>"
  pretty Add            = "+"
  pretty Subtract       = "-"
  pretty Multiply       = "*"
  pretty Divide         = "/"
  pretty Remainder      = "%"

instance PrettyPrinter AssignmentOperator where
  pretty :: AssignmentOperator -> Text
  pretty Assign              = "="
  pretty AddAssign           = "+="
  pretty SubAssign           = "-="
  pretty MulAssign           = "*="
  pretty DivAssign           = "/="
  pretty RemAssign           = "%="
  pretty BitAndAssign        = "&="
  pretty BitOrAssign         = "|="
  pretty BitXORAssign        = "^="
  pretty BitShiftLeftAssign  = "<<="
  pretty BitShiftRightAssign = ">>="

