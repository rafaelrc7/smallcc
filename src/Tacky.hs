module Tacky where

import qualified Parser.AST as P
import           Tacky.AST

translateTacky :: P.Program -> Program
translateTacky = snd . translateProgram emptyState

