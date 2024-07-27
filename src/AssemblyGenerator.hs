module AssemblyGenerator where

import           AssemblyGenerator.AST
import qualified Parser.AST            as P

translate :: P.Program -> Program
translate = translateProgram

