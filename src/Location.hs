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
  deriving Show

class Locatable a where
  locate :: a -> Location

  getLine :: a -> Line
  getLine a = let (Location line _ _) = locate a in line

  getColumn :: a -> Column
  getColumn a = let (Location _ column _) = locate a in column

  getBufferName :: a -> Maybe Text
  getBufferName a = let (Location _ _ bufferName) = locate a in bufferName

instance PrettyPrinter Location where
  pretty :: Location -> Text
  pretty (Location line col buff) = buff' <> pretty line <> ":" <> pretty col
    where buff' = case buff of Nothing -> ""
                               Just b  -> b <> ":"

