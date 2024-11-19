module Tacky where

import qualified Parser.AST                             as P
import           SemanticAnalyzer.SemanticAnalyzerMonad (LabelResolvingPhase)
import           Tacky.AST                              (Program,
                                                         translateProgram)

translateTacky :: P.Program LabelResolvingPhase -> Program
translateTacky = translateProgram

