module Tacky where

import qualified Parser.AST                             as P
import           SemanticAnalyzer.SemanticAnalyzerMonad (SwitchResolvingPhase)
import           Tacky.AST                              (Program,
                                                         translateProgram)

translateTacky :: P.Program SwitchResolvingPhase -> Program
translateTacky = translateProgram

