module Error where

import           Pretty       (PrettyPrinter (..))

import           Data.Text.IO (hPutStrLn)
import           System.IO    (stderr)

class (PrettyPrinter a) => Error a where
  putError :: a -> IO ()
  putError = hPutStrLn stderr . pretty

