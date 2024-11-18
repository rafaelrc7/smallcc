module Tacky where

import qualified SemanticAnalyzer.AST as SA
import           Tacky.AST

translateTacky :: SA.Program -> Program
translateTacky = translateProgram

