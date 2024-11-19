{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser.AST where

import           Data.Text       (Text)
import qualified Data.Text       as T
import           GHC.Types       (Constraint, Type)
import           Numeric.Natural (Natural)
import           Pretty          (PrettyPrinter (..), identLines)

type Identifier = Text


-- AST Definition --

newtype Program p = Program (FunctionDefinition p)
deriving instance (Forall Show p) => Show (Program p)

data FunctionDefinition p = Function Identifier (Block p)
deriving instance (Forall Show p) => Show (FunctionDefinition p)

data BlockItem p = BlockDeclaration (Declaration p)
                 | BlockStatement   (Statement p)
            -- | BlockLabel Label -- C23
            -- | BlockStatement UnlabeledStatement -- C23
deriving instance (Forall Show p) => Show (BlockItem p)

newtype Block p = Block [BlockItem p]
deriving instance (Forall Show p) => Show (Block p)

data Statement p = LabeledStatement   (Label p) (Statement p)
                 | UnlabeledStatement (UnlabeledStatement p)
deriving instance (Forall Show p) => Show (Statement p)

data UnlabeledStatement p = Expression (XExpression p) (Exp p)
                          | Compound   (XCompound   p) (Block p)
                          | If         (XIf         p) (Exp p) (Statement p) (Maybe (Statement p))
                          | Switch     (XSwitch     p) (Exp p) (Statement p)
                          | While      (XWhile      p) (Exp p) (Statement p)
                          | DoWhile    (XDoWhile    p) (Statement p) (Exp p)
                          | For        (XFor        p) (ForInit p) (Maybe (Exp p)) (Maybe (Exp p)) (Statement p)
                          | Goto       (XGoto       p) Identifier
                          | Continue   (XContinue   p)
                          | Break      (XBreak      p)
                          | Return     (XReturn     p) (Exp p)
                          | Null       (XNull       p)
deriving instance (Forall Show p) => Show (UnlabeledStatement p)

data Label p = Label   (XLabel   p) Identifier
             | Case    (XCase    p) Constant
             | Default (XDefault p)
deriving instance (Forall Show p) => Show (Label p)

data ForInit p = InitDecl (Declaration p)
               | InitExp  (Maybe (Exp p))
deriving instance (Forall Show p) => Show (ForInit p)

data Declaration p = Declaration (XDeclaration p) Identifier (Maybe (Exp p))
deriving instance (Forall Show p) => Show (Declaration p)

data Exp p = Constant    (XConstant    p) Constant
           | Var         (XVar         p) Identifier
           | Unary       (XUnary       p) UnaryOperator (Exp p)
           | Binary      (XBinary      p) BinaryOperator (Exp p) (Exp p)
           | Assignment  (XAssignment  p) (Exp p) AssignmentOperator (Exp p)
           | Conditional (XConditional p) (Exp p) (Exp p) (Exp p)
deriving instance (Forall Show p) => Show (Exp p)

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


-- Binary Operator Precedence --

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


-- AST Decorators --

type Forall (c :: Type -> Constraint) p = (c (XExpression p), c (XCompound p), c (XIf p), c (XSwitch p), c (XWhile p), c (XDoWhile p), c (XFor p), c (XGoto p), c (XContinue p), c (XBreak p), c (XReturn p), c (XNull p), c (XLabel p), c (XCase p), c (XDefault p), c (XConstant p), c (XVar p), c (XUnary p), c (XBinary p), c (XAssignment p), c (XConditional p), c (XDeclaration p))

type family XExpression  p
type family XCompound    p
type family XIf          p
type family XSwitch      p
type family XWhile       p
type family XDoWhile     p
type family XFor         p
type family XGoto        p
type family XContinue    p
type family XBreak       p
type family XReturn      p
type family XNull        p
type family XLabel       p
type family XCase        p
type family XDefault     p
type family XConstant    p
type family XVar         p
type family XUnary       p
type family XBinary      p
type family XAssignment  p
type family XConditional p
type family XDeclaration p


-- PrettyPrinter Instance --

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
  pretty (BlockStatement stmt)  = pretty stmt
  pretty (BlockDeclaration dec) = pretty dec
  -- pretty (BlockLabel label) = pretty label

instance PrettyPrinter (Statement p) where
  pretty :: Statement p -> Text
  pretty (LabeledStatement label stmt) = pretty label <> "\n" <> pretty stmt
  pretty (UnlabeledStatement stmt)     = pretty stmt

instance PrettyPrinter (UnlabeledStatement p) where
  pretty :: UnlabeledStatement p -> Text
  pretty (Return _ expr) = ret <> expr' <> ";\n"
    where ret = "return "
          expr' = pretty expr
  pretty (Expression _ expr) = pretty expr <> ";\n"
  pretty (If _ cond expThen Nothing) = "if (" <> pretty cond <> ")\n" <> identLines (pretty expThen)
  pretty (If _ cond expThen (Just expElse)) = "if (" <> pretty cond <> ")\n" <> identLines (pretty expThen) <> "\nelse\n" <> identLines (pretty expElse)
  pretty (Compound _ block) = "{\n" <> pretty block <> "\n}\n"
  pretty (Goto _ label) = "goto " <> pretty label <> ";\n"
  pretty (Break _) = "break;\n"
  pretty (Continue _) = "continue;\n"
  pretty (While _ cond body) = "while (" <> pretty cond <> ")" <> pretty body
  pretty (DoWhile _ body cond) = "do" <> pretty body <> "while (" <> pretty cond <> ");\n"
  pretty (For _ ini cond post body) = "for (" <> pretty ini <> "; " <> maybe "" pretty cond <> "; " <> maybe "" pretty post <> ")\n" <> pretty body
  pretty (Switch _ expr body) = "switch (" <> pretty expr <> ")\n" <> pretty body
  pretty (Null _) = ";\n"

instance PrettyPrinter (Label p) where
  pretty :: Label p -> Text
  pretty (Label _ label) = pretty label <> ":"
  pretty (Default _)     = "default:"
  pretty (Case _ expr)   = "case " <> pretty expr <> ":"

instance PrettyPrinter (ForInit p) where
  pretty :: ForInit p -> Text
  pretty (InitDecl decl) = pretty decl
  pretty (InitExp  expr) = maybe "" pretty expr

instance PrettyPrinter (Declaration p) where
  pretty :: Declaration p -> Text
  pretty (Declaration _ name Nothing) = "int " <> name <> ";\n";
  pretty (Declaration _ name (Just i)) = "int " <> name <> " = " <> pretty i <> ";\n";

instance PrettyPrinter (Exp p) where
  pretty :: Exp p -> Text
  pretty (Constant _ val) = pretty val
  pretty (Var _ var) = var
  pretty (Unary _ op@(UnaryAssignmentOperator PostDecrement) expr) = "(" <> pretty expr <> pretty op <> ")"
  pretty (Unary _ op@(UnaryAssignmentOperator PostIncrement) expr) = "(" <> pretty expr <> pretty op <> ")"
  pretty (Unary _ op expr) = "(" <> pretty op <> pretty expr <> ")"
  pretty (Binary _ op exprl exprr) = "(" <> T.intercalate " " [pretty exprl, pretty op, pretty exprr] <> ")"
  pretty (Assignment _ exprl op exprr) = "(" <> T.intercalate " " [pretty exprl, pretty op, pretty exprr] <> ")"
  pretty (Conditional _ cond exp1 exp2) = "(" <> pretty cond <> " ? " <> pretty exp1 <> " : " <> pretty exp2 <> ")"

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

