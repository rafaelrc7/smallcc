{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import           Data.Char         (isAlpha, isDigit, isSpace)
import           Data.Int          (Int64)
import qualified Data.Text         as T
import qualified Data.Text.Lazy    as L
import qualified Data.Text.Lazy.IO as LIO
import qualified Data.Text.Read    as R

import           Lexer.Token

data Lexer = Lexer { lexerSourceBuffer     :: L.Text
                   , lexerRemainingBuffer  :: L.Text
                   , lexerCurrLexeme       :: T.Text
                   , lexerCurrLexemeOffset :: Int64
                   , lexerCurrLexemeLength :: Int64
                   }

fromFile :: FilePath -> IO Lexer
fromFile filePath =
  LIO.readFile filePath >>= \sourceBuffer ->
  return $ Lexer { lexerSourceBuffer = sourceBuffer
                 , lexerRemainingBuffer = sourceBuffer
                 , lexerCurrLexeme = T.empty
                 , lexerCurrLexemeOffset = 0
                 , lexerCurrLexemeLength = 0
                 }

data LexerError = UnknownToken { unknownLexeme       :: T.Text
                               , unknownLexemeLine   :: Int64
                               , unknownLexemeColumn :: Int64
                               }
                | InternalError String
                | EOF
  deriving (Show)

scanUntilEOF :: Lexer -> Either LexerError [Token]
scanUntilEOF lexer = case token of
                       Right token' -> scanUntilEOF lexer' >>= \tokens' ->
                                       Right $ token' : tokens'
                       Left EOF -> Right []
                       Left err -> Left err
  where (token, lexer') = scanNextToken lexer

scanNextToken :: Lexer -> (Either LexerError Token, Lexer)
scanNextToken lexer | isAtEnd lexer = (Left EOF, lexer)
                    | isSpace symbol = scanNextToken $ advanceLexeme lexer'
                    | isDigit symbol = scanConstant lexer'
                    | isAlpha_ symbol = scanIdentifier lexer'
                    | otherwise = case symbol of
                                    '(' -> retToken OpenParens
                                    ')' -> retToken CloseParens
                                    '{' -> retToken OpenBrace
                                    '}' -> retToken CloseBrace
                                    ';' -> retToken Semicolon
                                    _   -> consumeUnknownToken lexer'
  where (symbol, lexer') = scanNextSymbol lexer
        retToken :: Token -> (Either LexerError Token, Lexer)
        retToken token = (Right token, advanceLexeme lexer')

scanConstant :: Lexer -> (Either LexerError Token, Lexer)
scanConstant lexer = maybe retToken scanConstant' (peek lexer)
  where scanConstant' :: Char -> (Either LexerError Token, Lexer)
        scanConstant' peekedSymbol
                        | isDigit peekedSymbol = scanConstant lexer'
                        | isBoundary peekedSymbol = retToken
                        | otherwise = consumeUnknownToken lexer'
          where (_, lexer') = scanNextSymbol lexer

        retToken :: (Either LexerError Token, Lexer)
        retToken = case R.decimal $ lexerCurrLexeme lexer of
                           Right (value, _) -> (Right $ Constant value, lexer)
                           Left err         -> (Left $ InternalError err, lexer)



scanIdentifier :: Lexer -> (Either LexerError Token, Lexer)
scanIdentifier lexer = maybe retToken scanIdentifier' (peek lexer)
  where scanIdentifier' :: Char -> (Either LexerError Token, Lexer)
        scanIdentifier' peekedSymbol
                          | isAlphaNum_ peekedSymbol = scanIdentifier lexer'
                          | isBoundary peekedSymbol = retToken
                          | otherwise = consumeUnknownToken lexer'
          where (_, lexer') = scanNextSymbol lexer

        retToken :: (Either LexerError Token, Lexer)
        retToken = case parseKeyword lexeme of
                     Just keyword -> (Right $ Keyword keyword, lexer)
                     Nothing      -> (Right $ Identifier lexeme, lexer)

        lexeme = lexerCurrLexeme lexer

consumeUnknownToken :: Lexer -> (Either LexerError Token, Lexer)
consumeUnknownToken lexer = maybe err consumeUnknownToken' (peek lexer)
  where consumeUnknownToken' :: Char -> (Either LexerError Token, Lexer)
        consumeUnknownToken' peekedSymbol
                               | isBoundary peekedSymbol = err
                               | otherwise = consumeUnknownToken lexer'
          where (_, lexer') = scanNextSymbol lexer

        err :: (Either LexerError Token, Lexer)
        err = unknownToken lexer

unknownToken :: Lexer -> (Either LexerError Token, Lexer)
unknownToken lexer = (Left err, advanceLexeme lexer)
  where (line, col) = getLexemeLineColumn
        err = UnknownToken {unknownLexeme=lexerCurrLexeme lexer, unknownLexemeLine=line, unknownLexemeColumn=col}

        getLexemeLineColumn :: (Int64, Int64)
        getLexemeLineColumn = (lexemeLine, lexemeColumn)
          where offset = lexerCurrLexemeOffset lexer
                sourceBufferUntilLexeme = L.take offset $ lexerSourceBuffer lexer
                lexemeLine = 1 + L.count "\n" sourceBufferUntilLexeme
                lexemeColumn = 1 + L.length (L.takeWhileEnd (/= '\n') sourceBufferUntilLexeme)

scanNextSymbol :: Lexer -> (Char, Lexer)
scanNextSymbol lexer = (hd, lexer { lexerCurrLexemeLength = lexerCurrLexemeLength lexer + 1
                                  , lexerRemainingBuffer = remainingBuffer'
                                  , lexerCurrLexeme = lexerCurrLexeme lexer `T.snoc` hd })
  where remainingBuffer = lexerRemainingBuffer lexer
        hd = L.head remainingBuffer
        remainingBuffer' = L.tail remainingBuffer

advanceLexeme :: Lexer -> Lexer
advanceLexeme lexer = lexer { lexerCurrLexemeOffset = offset + len
                            , lexerCurrLexemeLength = 0
                            , lexerCurrLexeme = T.empty }
  where offset = lexerCurrLexemeOffset lexer
        len = lexerCurrLexemeLength lexer

isAtEnd :: Lexer -> Bool
isAtEnd = L.null . lexerRemainingBuffer

lookAhead :: Lexer -> Int64 -> Maybe Char
lookAhead lexer index | index >= L.length remainingBuffer = Nothing
                      | otherwise = Just $ L.index remainingBuffer index
  where remainingBuffer = lexerRemainingBuffer lexer

peek :: Lexer -> Maybe Char
peek lexer = lookAhead lexer 0

isAlpha_ :: Char -> Bool
isAlpha_ '_' = True
isAlpha_ c   = isAlpha c

isAlphaNum_ :: Char -> Bool
isAlphaNum_ '_' = True
isAlphaNum_ c   = isAlpha c || isDigit c

isBoundary :: Char -> Bool
isBoundary = not . isAlphaNum_

