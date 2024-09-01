{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.PrettyPrint where

import           Data.Text  (Text)
import qualified Data.Text  as T

import           Parser.AST

identLines :: Text -> Text
identLines = T.unlines . map (T.cons '\t') . T.lines

padLeft :: Int -> Text -> Text
padLeft size = T.unlines . map (T.append pad) . T.lines
  where pad = T.replicate size " "

class PrettyPrinter a where
  pretty :: a -> Text

instance PrettyPrinter Program where
  pretty :: Program -> Text
  pretty (Program f) = "Program\n" <>  f'
    where f' = identLines $ pretty f

instance PrettyPrinter FunctionDefinition where
  pretty :: FunctionDefinition -> Text
  pretty (Function {funcBody=body, funcName=name}) = "Function '" <> name <> "'\n"
                                          <> body'
    where body' = T.concat $ map (identLines . pretty) body

instance PrettyPrinter BlockItem where
  pretty :: BlockItem -> Text
  pretty (Stmt stmt) = pretty stmt
  pretty (Dec dec)   = pretty dec

instance PrettyPrinter Statement where
  pretty :: Statement -> Text
  pretty (Return expr) = ret <> expr' <> ";\n"
    where ret = "return "
          expr' = pretty expr
  pretty (Expression expr) = pretty expr <> ";\n"
  pretty Null = ";\n"

instance PrettyPrinter Declaration where
  pretty :: Declaration -> Text
  pretty (Declaration name Nothing) = "int " <> name <> ";\n";
  pretty (Declaration name (Just i)) = "int " <> name <> " = " <> pretty i <> ";\n";

instance PrettyPrinter Exp where
  pretty :: Exp -> Text
  pretty (Constant val) = pretty val
  pretty (Unary op expr) = pretty op <> pretty expr
  pretty (Binary op exprl exprr) = "(" <> T.intercalate " " [pretty exprl, pretty op, pretty exprr] <> ")"
  pretty (Var var) = var
  pretty (Assignment lhs rhs) = "(" <> lhs' <> " = " <> rhs' <> ")"
    where lhs' = pretty lhs
          rhs' = pretty rhs

instance PrettyPrinter Constant where
  pretty :: Constant -> Text
  pretty (CInt val) = T.pack $ show val

instance PrettyPrinter UnaryOperator where
  pretty :: UnaryOperator -> Text
  pretty Complement = "~"
  pretty Negate     = "-"
  pretty Not        = "!"

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
  pretty Assign         = "="

