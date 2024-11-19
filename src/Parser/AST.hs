{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parser.AST where

import           Data.Text       (Text)
import qualified Data.Text       as T
import           Numeric.Natural (Natural)
import           Pretty          (PrettyPrinter (..), identLines)

type Identifier = Text

newtype Program = Program FunctionDefinition
  deriving (Show)

data FunctionDefinition = Function Identifier Block
  deriving (Show)

data BlockItem = BlockDeclaration Declaration
               | BlockStatement Statement
            -- | BlockLabel Label -- C23
            -- | BlockStatement UnlabeledStatement -- C23
  deriving (Show)

newtype Block = Block [BlockItem]
  deriving (Show)

data Statement = LabeledStatement Label Statement
               | UnlabeledStatement UnlabeledStatement
  deriving (Show)

data UnlabeledStatement = Expression Exp
                        | Compound Block
                        | If Exp Statement (Maybe Statement)
                        | Switch Exp Statement
                        | While Exp Statement
                        | DoWhile Statement Exp
                        | For ForInit (Maybe Exp) (Maybe Exp) Statement
                        | Goto Identifier
                        | Continue
                        | Break
                        | Return Exp
                        | Null
  deriving (Show)

data Label = Label Identifier
           | Case Constant
           | Default
  deriving (Show)

data ForInit = InitDecl Declaration
             | InitExp  (Maybe Exp)
  deriving (Show)

data Declaration = Declaration Identifier (Maybe Exp)
  deriving (Show)

data Exp = Constant Constant
         | Var Identifier
         | Unary UnaryOperator Exp
         | Binary BinaryOperator Exp Exp
         | Assignment Exp AssignmentOperator Exp
         | Conditional Exp Exp Exp
  deriving (Show)

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

instance PrettyPrinter Program where
  pretty :: Program -> Text
  pretty (Program f) = "Program\n" <>  f'
    where f' = identLines $ pretty f

instance PrettyPrinter FunctionDefinition where
  pretty :: FunctionDefinition -> Text
  pretty (Function name body) = "Function '" <> name <> "'\n" <> pretty body <> "\n"

instance PrettyPrinter Block where
  pretty :: Block -> Text
  pretty (Block blockItens) = T.concat $ map (identLines . pretty) blockItens

instance PrettyPrinter BlockItem where
  pretty :: BlockItem -> Text
  pretty (BlockStatement stmt)  = pretty stmt
  pretty (BlockDeclaration dec) = pretty dec
  -- pretty (BlockLabel label) = pretty label

instance PrettyPrinter Statement where
  pretty :: Statement -> Text
  pretty (LabeledStatement label stmt) = pretty label <> "\n" <> pretty stmt
  pretty (UnlabeledStatement stmt)     = pretty stmt

instance PrettyPrinter UnlabeledStatement where
  pretty :: UnlabeledStatement -> Text
  pretty (Return expr) = ret <> expr' <> ";\n"
    where ret = "return "
          expr' = pretty expr
  pretty (Expression expr) = pretty expr <> ";\n"
  pretty (If cond expThen Nothing) = "if (" <> pretty cond <> ")\n" <> identLines (pretty expThen)
  pretty (If cond expThen (Just expElse)) = "if (" <> pretty cond <> ")\n" <> identLines (pretty expThen) <> "\nelse\n" <> identLines (pretty expElse)
  pretty (Compound block) = "{\n" <> pretty block <> "\n}\n"
  pretty (Goto label) = "goto " <> pretty label <> ";\n"
  pretty Break = "break;\n"
  pretty Continue = "continue;\n"
  pretty (While cond body) = "while (" <> pretty cond <> ")" <> pretty body
  pretty (DoWhile body cond) = "do" <> pretty body <> "while (" <> pretty cond <> ");\n"
  pretty (For ini cond post body) = "for (" <> pretty ini <> "; " <> maybe "" pretty cond <> "; " <> maybe "" pretty post <> ")\n" <> pretty body
  pretty (Switch expr body) = "switch (" <> pretty expr <> ")\n" <> pretty body
  pretty Null = ";\n"

instance PrettyPrinter Label where
  pretty :: Label -> Text
  pretty (Label label) = pretty label <> ":"
  pretty Default       = "default:"
  pretty (Case expr)   = "case " <> pretty expr <> ":"

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

