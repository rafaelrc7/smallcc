{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import           Data.Char         (isAlpha, isDigit, isSpace)
import           Data.Int          (Int64)
import qualified Data.Text         as T
import qualified Data.Text.Lazy    as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.Read    as T

import           Lexer.Token

data Lexer = Lexer { lexerReadBuffer      :: L.Text
                   , lexerRemainingBuffer :: L.Text
                   }
  deriving (Show)

newtype Lexeme = Lexeme T.Text
  deriving (Show)

data LexerError = UnknownToken { unknownLexeme       :: T.Text
                               , unknownLexemeLine   :: Int64
                               , unknownLexemeColumn :: Int64
                               }
                | InternalError String
                | EOF
  deriving (Show)

fromFile :: FilePath -> IO Lexer
fromFile filePath =
  L.readFile filePath >>= \sourceBuffer ->
  return $ Lexer { lexerRemainingBuffer = sourceBuffer
                 , lexerReadBuffer = L.empty
                 }

scanUntilEOF :: Lexer -> Either LexerError [Token]
scanUntilEOF lexer = case nextToken lexer of
                       Right (token, lexer') -> scanUntilEOF lexer' >>= \tokens' ->
                                                Right $ token : tokens'
                       Left EOF -> Right []
                       Left err -> Left err


nextToken :: Lexer -> Either LexerError (Token, Lexer)
nextToken lexer = scanToken lexer newLexeme
  where newLexeme = Lexeme T.empty

scanToken :: Lexer -> Lexeme -> Either LexerError (Token, Lexer)
scanToken lexer lexeme =
  case nextSymbol lexer lexeme of
    Nothing -> Left EOF
    Just (symbol, lexer', lexeme') ->
      case symbol of
        '(' -> retToken OpenParens
        ')' -> retToken CloseParens
        '{' -> retToken OpenBrace
        '}' -> retToken CloseBrace
        ';' -> retToken Semicolon
        '~' -> retToken Complement
        '+' -> retToken Plus
        '*' -> retToken Asterisk
        '/' -> retToken ForwardSlash
        '%' -> retToken Percent
        '-' -> scanMinusDecrement lexer' lexeme'
        _ | isSpace symbol -> nextToken lexer'
          | isDigit symbol -> scanConstant lexer' lexeme'
          | isAlpha_ symbol -> scanIdentifier lexer' lexeme'
          | otherwise -> scanUnknownToken lexer' lexeme'
      where retToken :: Token -> Either LexerError (Token, Lexer)
            retToken token = Right (token, lexer')

nextSymbol :: Lexer -> Lexeme -> Maybe (Char, Lexer, Lexeme)
nextSymbol lexer (Lexeme lexeme) =
  do (symbol, remainingBuffer) <- L.uncons $ lexerRemainingBuffer lexer
     let readBuffer = L.snoc (lexerReadBuffer lexer) symbol
     return (symbol
            , lexer { lexerRemainingBuffer = remainingBuffer
                    , lexerReadBuffer = readBuffer
                    }
            , Lexeme $ T.snoc lexeme symbol
            )

scanConstant :: Lexer -> Lexeme -> Either LexerError (Token, Lexer)
scanConstant lexer lexeme@(Lexeme lexemeBuffer) =
  case nextSymbol lexer lexeme of
    Nothing -> token
    Just (symbol, lexer', lexeme')
      | isBoundary symbol -> token
      | isDigit symbol -> scanConstant lexer' lexeme'
      | otherwise -> scanUnknownToken lexer' lexeme'
  where token :: Either LexerError (Token, Lexer)
        token = case T.decimal lexemeBuffer of
                  Right (value, _) -> Right (Constant value, lexer)
                  Left err         -> Left $ InternalError err

scanIdentifier :: Lexer -> Lexeme -> Either LexerError (Token, Lexer)
scanIdentifier lexer lexeme@(Lexeme lexemeBuffer) =
  case nextSymbol lexer lexeme of
    Nothing -> token
    Just (symbol, lexer', lexeme')
      | isAlphaNum_ symbol -> scanIdentifier lexer' lexeme'
      | otherwise -> token
  where token :: Either LexerError (Token, Lexer)
        token = case parseKeyword lexemeBuffer of
                  Just keyword -> Right (Keyword keyword, lexer)
                  Nothing      -> Right (Identifier lexemeBuffer, lexer)

scanMinusDecrement :: Lexer -> Lexeme -> Either LexerError (Token, Lexer)
scanMinusDecrement lexer lexeme =
  case nextSymbol lexer lexeme of
    Just ('-', lexer', _) -> Right (Decrement, lexer')
    _                     -> Right (Minus, lexer)

scanUnknownToken :: Lexer -> Lexeme -> Either LexerError (Token, Lexer)
scanUnknownToken lexer lexeme =
  case nextSymbol lexer lexeme of
    Nothing -> err
    Just (symbol, lexer', lexeme') | isBoundary symbol -> err
                                   | otherwise -> scanUnknownToken lexer' lexeme'
  where err = Left $ unknownToken lexer lexeme

unknownToken :: Lexer -> Lexeme -> LexerError
unknownToken lexer lexeme@(Lexeme lexemeBuff) =
  UnknownToken { unknownLexeme = lexemeBuff
               , unknownLexemeLine = line
               , unknownLexemeColumn = col
               }
  where (line, col) = getLexemeLineColumn lexer lexeme

getLexemeLineColumn :: Lexer -> Lexeme -> (Int64, Int64)
getLexemeLineColumn lexer (Lexeme lexeme) = (lexemeLine, lexemeColumn)
  where readBuffer = lexerReadBuffer lexer
        bufferUntilLexeme = L.dropEnd (fromIntegral $ T.length lexeme) readBuffer
        lexemeLine = 1 + L.count "\n" bufferUntilLexeme
        lexemeColumn = 1 + L.length (L.takeWhileEnd (/= '\n') bufferUntilLexeme)

isAtEnd :: Lexer -> Bool
isAtEnd = L.null . lexerRemainingBuffer

lookAhead :: Lexer -> Int64 -> Maybe Char
lookAhead lexer index | index >= L.length remainingBuffer = Nothing
                      | otherwise = Just $ L.index remainingBuffer index
  where remainingBuffer = lexerRemainingBuffer lexer

isAlpha_ :: Char -> Bool
isAlpha_ '_' = True
isAlpha_ c   = isAlpha c

isAlphaNum_ :: Char -> Bool
isAlphaNum_ '_' = True
isAlphaNum_ c   = isAlpha c || isDigit c

isBoundary :: Char -> Bool
isBoundary = not . isAlphaNum_

