{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pretty where

import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Int (Int64)

class PrettyPrinter a where
  pretty :: a -> Text

instance PrettyPrinter Text where
  pretty :: Text -> Text
  pretty = id

instance PrettyPrinter Int where
  pretty :: Int -> Text
  pretty = pack . show

instance PrettyPrinter Int64 where
  pretty :: Int64 -> Text
  pretty = pack . show

identLines :: Text -> Text
identLines = T.unlines . map (T.cons '\t') . T.lines

padLeft :: Int -> Text -> Text
padLeft size = T.unlines . map (T.append pad) . T.lines
  where pad = T.replicate size " "

