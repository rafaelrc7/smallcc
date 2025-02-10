module AssemblyEmitter where

import           AssemblyEmitter.AST
import           AssemblyGenerator.AST
import           Control.Monad.Reader  (runReaderT)
import           Control.Monad.Writer  (runWriter)
import qualified Data.Set              as S
import           Data.Text             (Text)

emitAssembly :: Program -> Text
emitAssembly program = snd . runWriter $ runReaderT (emit program) S.empty
