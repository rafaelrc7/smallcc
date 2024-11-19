{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SemanticAnalyzer.AST where


import           Data.Text  (Text)
import qualified Data.Text  as T
import           Parser.AST (AssignmentOperator (..), BinaryOperator (..),
                             Constant (..), Identifier,
                             UnaryAssignmentOperator (..), UnaryOperator (..))
import           Pretty     (PrettyPrinter (..), identLines)

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
                        | While Exp Statement Identifier
                        | DoWhile Statement Exp Identifier
                        | For ForInit (Maybe Exp) (Maybe Exp) Statement Identifier
                        | Goto Identifier
                        | Continue Identifier
                        | Break Identifier
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
  pretty (Break _) = "break;\n"
  pretty (Continue _) = "continue;\n"
  pretty (While cond body _) = "while (" <> pretty cond <> ")" <> pretty body
  pretty (DoWhile body cond _) = "do" <> pretty body <> "while (" <> pretty cond <> ");\n"
  pretty (For ini cond post body _) = "for (" <> pretty ini <> "; " <> maybe "" pretty cond <> "; " <> maybe "" pretty post <> ")\n" <> pretty body
  pretty (Switch expr body) = "switch (" <> pretty expr <> ")\n" <> pretty body
  pretty Null = ";\n"

instance PrettyPrinter ForInit where
  pretty :: ForInit -> Text
  pretty (InitDecl decl) = pretty decl
  pretty (InitExp  expr) = maybe "" pretty expr

instance PrettyPrinter Label where
  pretty :: Label -> Text
  pretty (Label label) = pretty label <> ":"
  pretty Default       = "default:"
  pretty (Case expr)   = "case " <> pretty expr <> ":"

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

