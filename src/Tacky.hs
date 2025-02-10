module Tacky where

import qualified Parser.AST                             as P
import           SemanticAnalyzer.SemanticAnalyzerMonad (TypeCheckingPhase)
import           Tacky.AST                              (Program,
                                                         translateProgram)

translateTacky :: P.Program TypeCheckingPhase -> Program
translateTacky = translateProgram
