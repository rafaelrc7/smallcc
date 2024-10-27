{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Error (ParserError(..)
                    , expectedEOF
                    , unexpectedEOF
                    , unexpectedToken
                    ) where

import           Data.Set    (Set)
import qualified Data.Set    as S
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Error       (Error)
import           Lexer.Token (Token (..))
import           Location    (Locatable (..), Location)
import           Pretty      (PrettyPrinter (..))

data ParserError = UnexpectedToken (Set Text) (Maybe Token)
  deriving (Show)

expectedEOF :: Token -> ParserError
expectedEOF t = UnexpectedToken (S.singleton "EOF") (Just t)

unexpectedEOF :: Text -> ParserError
unexpectedEOF e = UnexpectedToken (S.singleton e) Nothing

unexpectedToken :: Text -> Token -> ParserError
unexpectedToken e t = UnexpectedToken (S.singleton e) (Just t)

instance Locatable ParserError where
  locate :: ParserError -> Maybe Location
  locate (UnexpectedToken _ (Just (Token _ _ loc))) = Just loc
  locate (UnexpectedToken _ Nothing)                = Nothing

instance PrettyPrinter ParserError where
  pretty :: ParserError -> Text
  pretty (UnexpectedToken e (Just (Token g _ _))) = "Expected " <> printExpectedSet e <> " but got a '" <> pretty g <> "'"
  pretty (UnexpectedToken e Nothing)              = "Expected " <> printExpectedSet e <> " but reached EOF"

printExpectedSet :: Set Text -> Text
printExpectedSet ex | S.null ex = "token 'EOF'"
                    | S.size ex == 1 = "token '" <> S.elemAt 0 ex <> "'"
                    | otherwise = "one of tokens [" <> T.intercalate ", " (map (\t -> T.cons '\'' $ T.snoc t '\'') $ S.elems ex) <> "]"

instance Semigroup ParserError where
  (<>) :: ParserError -> ParserError -> ParserError
  UnexpectedToken e1 t1 <> UnexpectedToken e2 t2
    | t1 == t2 = UnexpectedToken (S.union e1 e2) t1
    | S.size e1 < S.size e2 = UnexpectedToken e2 t2
    | otherwise = UnexpectedToken e1 t1

instance Monoid ParserError where
  mempty :: ParserError
  mempty = UnexpectedToken S.empty Nothing

instance Error ParserError

