{-# LANGUAGE OverloadedStrings #-}

module Error where

import           Data.Text.IO (hPutStrLn)
import           Location     (Locatable (..))
import           Pretty       (PrettyPrinter (..))
import           System.IO    (stderr)

class (PrettyPrinter a, Locatable a) => Error a where
  putError :: a -> IO ()
  putError err = hPutStrLn stderr $ loc <> "Error: " <> pretty err
    where
      loc = maybe "" (\loc' -> pretty loc' <> ":") (locate err)
