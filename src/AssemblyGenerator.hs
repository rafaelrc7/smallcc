module AssemblyGenerator where

import           AssemblyGenerator.AST
import qualified Tacky.AST             as T

translate :: T.Program -> Program
translate = translateProgram
