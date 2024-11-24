module AssemblyEmitter where

import           Data.Text             (Text)

import           AssemblyEmitter.AST
import           AssemblyGenerator.AST
import           Control.Monad.Reader  (runReaderT)
import           Control.Monad.Writer  (runWriter)
import qualified Data.Set              as S

emitAssembly :: Program -> Text
emitAssembly program = snd . runWriter $ runReaderT (emit program) S.empty

