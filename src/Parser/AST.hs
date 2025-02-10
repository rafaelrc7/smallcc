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

newtype Program p = Program [FunctionDeclaration p]

deriving instance (Forall Show p) => Show (Program p)

deriving instance (Forall Eq p) => Eq (Program p)

deriving instance (Forall Ord p) => Ord (Program p)

data Declaration p
  = FunDecl (FunctionDeclaration p)
  | VarDecl (VarDeclaration p)

deriving instance (Forall Show p) => Show (Declaration p)

deriving instance (Forall Eq p) => Eq (Declaration p)

deriving instance (Forall Ord p) => Ord (Declaration p)

data FunctionDeclaration p = FunctionDeclaration (XFunDecl p) Identifier [Identifier] (Maybe (Block p))

deriving instance (Forall Show p) => Show (FunctionDeclaration p)

deriving instance (Forall Eq p) => Eq (FunctionDeclaration p)

deriving instance (Forall Ord p) => Ord (FunctionDeclaration p)

data VarDeclaration p = VarDeclaration (XVarDecl p) Identifier (Maybe (Exp p))

deriving instance (Forall Show p) => Show (VarDeclaration p)

deriving instance (Forall Eq p) => Eq (VarDeclaration p)

deriving instance (Forall Ord p) => Ord (VarDeclaration p)

data BlockItem p
  = BlockDeclaration (Declaration p)
  | BlockStatement (Statement p)

-- \| BlockLabel Label -- C23
-- \| BlockStatement UnlabeledStatement -- C23
deriving instance (Forall Show p) => Show (BlockItem p)

deriving instance (Forall Eq p) => Eq (BlockItem p)

deriving instance (Forall Ord p) => Ord (BlockItem p)

newtype Block p = Block [BlockItem p]

deriving instance (Forall Show p) => Show (Block p)

deriving instance (Forall Eq p) => Eq (Block p)

deriving instance (Forall Ord p) => Ord (Block p)

data Statement p
  = LabeledStatement (Label p) (Statement p)
  | UnlabeledStatement (UnlabeledStatement p)

deriving instance (Forall Show p) => Show (Statement p)

deriving instance (Forall Eq p) => Eq (Statement p)

deriving instance (Forall Ord p) => Ord (Statement p)

data UnlabeledStatement p
  = Expression (XExpression p) (Exp p)
  | Compound (XCompound p) (Block p)
  | If (XIf p) (Exp p) (Statement p) (Maybe (Statement p))
  | Switch (XSwitch p) (Exp p) (Statement p)
  | While (XWhile p) (Exp p) (Statement p)
  | DoWhile (XDoWhile p) (Statement p) (Exp p)
  | For (XFor p) (ForInit p) (Maybe (Exp p)) (Maybe (Exp p)) (Statement p)
  | Goto (XGoto p) Identifier
  | Continue (XContinue p)
  | Break (XBreak p)
  | Return (XReturn p) (Exp p)
  | Null (XNull p)

deriving instance (Forall Show p) => Show (UnlabeledStatement p)

deriving instance (Forall Eq p) => Eq (UnlabeledStatement p)

deriving instance (Forall Ord p) => Ord (UnlabeledStatement p)

data Label p
  = Label (XLabel p) Identifier
  | Case (XCase p) (XCaseV p)
  | Default (XDefault p)

deriving instance (Forall Show p) => Show (Label p)

deriving instance (Forall Eq p) => Eq (Label p)

deriving instance (Forall Ord p) => Ord (Label p)

data ForInit p
  = InitDecl (VarDeclaration p)
  | InitExp (Maybe (Exp p))

deriving instance (Forall Show p) => Show (ForInit p)

deriving instance (Forall Eq p) => Eq (ForInit p)

deriving instance (Forall Ord p) => Ord (ForInit p)

data Exp p
  = Constant (XConstant p) Constant
  | Var (XVar p) Identifier
  | Unary (XUnary p) UnaryOperator (Exp p)
  | Binary (XBinary p) BinaryOperator (Exp p) (Exp p)
  | Assignment (XAssignment p) (Exp p) AssignmentOperator (Exp p)
  | Conditional (XConditional p) (Exp p) (Exp p) (Exp p)
  | FunctionCall (XFunctionCall p) Identifier [Exp p]

deriving instance (Forall Show p) => Show (Exp p)

deriving instance (Forall Eq p) => Eq (Exp p)

deriving instance (Forall Ord p) => Ord (Exp p)

newtype Constant = CInt Int
  deriving (Show, Eq, Ord)

data UnaryOperator
  = Complement
  | Negate
  | Not
  | UnaryAssignmentOperator UnaryAssignmentOperator
  deriving (Show, Eq, Ord)

data UnaryAssignmentOperator
  = PreDecrement
  | PreIncrement
  | PostDecrement
  | PostIncrement
  deriving (Show, Eq, Ord)

data BinaryOperator
  = Add
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
  deriving (Show, Eq, Ord)

data AssignmentOperator
  = Assign
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
  deriving (Show, Eq, Ord)

-- Binary Operator Precedence --

data Associativity = LeftAssociative | RightAssociative

data Precedence = Precedence Associativity Natural

-- https://en.cppreference.com/w/c/language/operator_precedence
precedence :: BinaryOperator -> Precedence
precedence Or             = Precedence LeftAssociative 5
precedence And            = Precedence LeftAssociative 10
precedence BitOr          = Precedence LeftAssociative 15
precedence BitXOR         = Precedence LeftAssociative 20
precedence BitAnd         = Precedence LeftAssociative 25
precedence EqualsTo       = Precedence LeftAssociative 30
precedence NotEqualsTo    = Precedence LeftAssociative 30
precedence Less           = Precedence LeftAssociative 35
precedence LessOrEqual    = Precedence LeftAssociative 35
precedence Greater        = Precedence LeftAssociative 35
precedence GreaterOrEqual = Precedence LeftAssociative 35
precedence BitShiftLeft   = Precedence LeftAssociative 40
precedence BitShiftRight  = Precedence LeftAssociative 40
precedence Add            = Precedence LeftAssociative 45
precedence Subtract       = Precedence LeftAssociative 45
precedence Multiply       = Precedence LeftAssociative 50
precedence Divide         = Precedence LeftAssociative 50
precedence Remainder      = Precedence LeftAssociative 50

-- AST Decorators --

type Forall (c :: Type -> Constraint) p = (c (XExpression p), c (XCompound p), c (XIf p), c (XSwitch p), c (XWhile p), c (XDoWhile p), c (XFor p), c (XGoto p), c (XContinue p), c (XBreak p), c (XReturn p), c (XNull p), c (XLabel p), c (XCase p), c (XCaseV p), c (XDefault p), c (XConstant p), c (XVar p), c (XUnary p), c (XBinary p), c (XAssignment p), c (XConditional p), c (XVarDecl p), c (XFunDecl p), c (XFunctionCall p))

type family XExpression p

type family XCompound p

type family XIf p

type family XSwitch p

type family XWhile p

type family XDoWhile p

type family XFor p

type family XGoto p

type family XContinue p

type family XBreak p

type family XReturn p

type family XNull p

type family XLabel p

type family XCase p

type family XCaseV p

type family XDefault p

type family XConstant p

type family XVar p

type family XUnary p

type family XBinary p

type family XAssignment p

type family XConditional p

type family XVarDecl p

type family XFunDecl p

type family XFunctionCall p

-- PrettyPrinter Instance --

instance (PrettyPrinter (XCaseV p)) => PrettyPrinter (Program p) where
  pretty :: (PrettyPrinter (XCaseV p)) => Program p -> Text
  pretty (Program f) = T.intercalate "\n" $ map pretty f

instance (PrettyPrinter (XCaseV p)) => PrettyPrinter (FunctionDeclaration p) where
  pretty :: (PrettyPrinter (XCaseV p)) => FunctionDeclaration p -> Text
  pretty (FunctionDeclaration _ name params body) =
    "int "
      <> name
      <> "("
      <> T.intercalate ", " (map pretty params)
      <> ")"
      <> maybe ";" (\b -> " {\n" <> pretty b <> "}") body
      <> "\n"

instance (PrettyPrinter (XCaseV p)) => PrettyPrinter (Block p) where
  pretty :: (PrettyPrinter (XCaseV p)) => Block p -> Text
  pretty (Block blockItens) = T.concat $ map (identLines . pretty) blockItens

instance (PrettyPrinter (XCaseV p)) => PrettyPrinter (BlockItem p) where
  pretty :: (PrettyPrinter (XCaseV p)) => BlockItem p -> Text
  pretty (BlockStatement stmt)  = pretty stmt
  pretty (BlockDeclaration dec) = pretty dec

-- pretty (BlockLabel label) = pretty label

instance (PrettyPrinter (XCaseV p)) => PrettyPrinter (Statement p) where
  pretty :: (PrettyPrinter (XCaseV p)) => Statement p -> Text
  pretty (LabeledStatement label stmt) = pretty label <> "\n" <> pretty stmt
  pretty (UnlabeledStatement stmt)     = pretty stmt

instance (PrettyPrinter (XCaseV p)) => PrettyPrinter (UnlabeledStatement p) where
  pretty :: (PrettyPrinter (XCaseV p)) => UnlabeledStatement p -> Text
  pretty (Return _ expr) = ret <> expr' <> ";\n"
    where
      ret = "return "
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

instance (PrettyPrinter (XCaseV p)) => PrettyPrinter (Label p) where
  pretty :: (PrettyPrinter (XCaseV p)) => Label p -> Text
  pretty (Label _ label) = pretty label <> ":"
  pretty (Default _)     = "default:"
  pretty (Case _ expr)   = "case " <> pretty expr <> ":"

instance PrettyPrinter (ForInit p) where
  pretty :: ForInit p -> Text
  pretty (InitDecl decl) = pretty decl
  pretty (InitExp expr)  = maybe "" pretty expr

instance (PrettyPrinter (XCaseV p)) => PrettyPrinter (Declaration p) where
  pretty :: Declaration p -> Text
  pretty (VarDecl decl) = pretty decl
  pretty (FunDecl decl) = pretty decl

instance PrettyPrinter (VarDeclaration p) where
  pretty :: VarDeclaration p -> Text
  pretty (VarDeclaration _ name Nothing) = "int " <> name <> ";\n"
  pretty (VarDeclaration _ name (Just i)) = "int " <> name <> " = " <> pretty i <> ";\n"

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
  pretty (FunctionCall _ name args) = name <> "(" <> T.intercalate ", " (map pretty args) <> ")"

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
