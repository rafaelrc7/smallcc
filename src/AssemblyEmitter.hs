module AssemblyEmitter where

import           Data.Text             (Text)

import           AssemblyEmitter.AST
import           AssemblyGenerator.AST

emitAssembly :: Program -> Text
emitAssembly = emit

