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
  pretty (Program f) = "Program\n" `T.append`  f'
    where f' = identLines $ pretty f

instance PrettyPrinter FunctionDefinition where
  pretty :: FunctionDefinition -> Text
  pretty (Function {funcBody=body, funcName=name}) = "Function '" `T.append` name `T.append` "'\n"
                                          `T.append` body'
    where body' = identLines $ pretty body

instance PrettyPrinter Statement where
  pretty :: Statement -> Text
  pretty (Return expr) = ret `T.append` expr' `T.append` "\n"
    where ret = "return "
          expr' = pretty expr

instance PrettyPrinter Exp where
  pretty :: Exp -> Text
  pretty (Constant val) = pretty val
  pretty (Unary op expr) = pretty op `T.append` pretty expr
  pretty (Binary op exprl exprr) = "(" `T.append` T.intercalate " " [pretty exprl, pretty op, pretty exprr] `T.append` ")"

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
  pretty Equals         = "=="
  pretty NotEquals      = "!="
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

