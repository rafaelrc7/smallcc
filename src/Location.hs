{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Location where

import           Pretty

import           Data.Int  (Int64)
import           Data.Text (Text)

type Line = Int64
type Column = Int64
type BufferName = Text

data Location = Location Line Column (Maybe Text)
  deriving (Show, Eq)

instance Ord Location where
  compare :: Location -> Location -> Ordering
  compare (Location lin1 col1 _) (Location lin2 col2 _)
    | lin1 < lin2 = LT
    | lin1 > lin2 = GT
    | col1 < col2 = LT
    | col1 > col2 = GT
    | otherwise   = EQ

class Locatable a where
  locate :: a -> Maybe Location

instance PrettyPrinter Location where
  pretty :: Location -> Text
  pretty (Location line col buff) = buff' <> pretty line <> ":" <> pretty col
    where buff' = case buff of Nothing -> ""
                               Just b  -> b <> ":"

