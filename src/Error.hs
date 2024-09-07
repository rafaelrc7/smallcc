{-# LANGUAGE OverloadedStrings #-}

module Error where

import           Pretty       (PrettyPrinter (..))

import           Data.Text.IO (hPutStrLn)
import           Location     (Locatable (..))
import           System.IO    (stderr)

class (PrettyPrinter a, Locatable a) => Error a where
  putError :: a -> IO ()
  putError err = hPutStrLn stderr $ pretty (locate err) <> ": Error: " <> pretty err

