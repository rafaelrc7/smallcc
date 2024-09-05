{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import           Lexer.Token

import           Control.Monad.Except (ExceptT, MonadError (throwError),
                                       handleError, runExceptT)
import           Control.Monad.State  (MonadState (get, put), State, evalState,
                                       modify)
import           Data.Char            (isAlpha, isDigit, isSpace)
import           Data.Int             (Int64)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as L
import qualified Data.Text.Lazy.IO    as L
import qualified Data.Text.Read       as T

-- WriterT ?
type LexerMonad a = ExceptT LexerError (State LexerState) a

type Lexeme = T.Text
type Buffer = L.Text

type LexerState = (Buffer, Buffer, Lexeme)

resetLexeme :: LexerState -> LexerState
resetLexeme (readBuff, remainBuff, _) = (readBuff, remainBuff, T.empty)

catchST :: (LexerError -> LexerMonad a) -> LexerMonad a -> LexerMonad a
catchST handler action = get >>= \st -> handleError (\e -> put st >> handler e) action

data LexerError = UnknownToken { unknownLexeme       :: T.Text
                               , unknownLexemeLine   :: Int64
                               , unknownLexemeColumn :: Int64
                               }
                | MalformedToken
                | MalformedConstant String
                | EOF
  deriving (Show)

fromFile :: FilePath -> IO Buffer
fromFile = L.readFile

scanUntilEOF :: Buffer -> Either LexerError [Token]
scanUntilEOF buffer = evalState (runExceptT scanUntilEOF') (L.empty, buffer, T.empty)
  where scanUntilEOF' :: LexerMonad [Token]
        scanUntilEOF' = handleError handler $ (:) <$> nextToken <*> scanUntilEOF'
          where handler :: LexerError -> LexerMonad [Token]
                handler EOF = return []
                handler e   = throwError e

nextToken :: LexerMonad Token
nextToken = modify resetLexeme >> scanToken

scanToken :: LexerMonad Token
scanToken = peekSymbol >>= \case
  Nothing  -> throwError EOF
  Just '(' -> consumeSymbol >> return OpenParens
  Just ')' -> consumeSymbol >> return CloseParens
  Just '{' -> consumeSymbol >> return OpenBrace
  Just '}' -> consumeSymbol >> return CloseBrace
  Just ';' -> consumeSymbol >> return Semicolon
  Just '~' -> consumeSymbol >> return Complement
  Just s | isSpace  s -> consumeSymbol >> nextToken
         | isDigit  s -> consumeSymbol >> scanConstant
         | isAlpha_ s -> consumeSymbol >> scanIdentifier
         | otherwise  -> scanCompoundToken [ ("=", Equals),       ("==", EqualsTo)
                                           , ("!", Not),          ("!=", NotEqualsTo)
                                           , ("+", Plus),         ("+=", IncAssign),      ("++", Increment)
                                           , ("-", Minus),        ("-=", DecAssign),      ("--", Decrement)
                                           , ("*", Asterisk),     ("*=", MulAssign)
                                           , ("/", ForwardSlash), ("/=", DivAssign)
                                           , ("%", Percent),      ("%=", ModAssign)
                                           , ("^", BitXOR),       ("^=", BitXORAssign)
                                           , ("&", BitAnd),       ("&=", BitAndAssign),   ("&&", And)
                                           , ("|", BitOr),        ("|=", BitOrAssign),    ("||", Or)
                                           , ("<", Less),         ("<=", LessOrEqual),    ("<<", BitShiftLeft),  ("<<=", BitShiftLeftAssign)
                                           , (">", Greater),      (">=", GreaterOrEqual), (">>", BitShiftRight), (">>=", BitShiftRightAssign)]

consumeSymbol :: LexerMonad (Maybe Char)
consumeSymbol = get >>= \(readBuff, remainBuff, lexeme) ->
  case L.uncons remainBuff of
    Nothing -> return Nothing
    Just (symbol, remainBuff') ->
      do put (L.snoc readBuff symbol, remainBuff', lexeme `T.snoc` symbol)
         return $ Just symbol

peekSymbol :: LexerMonad (Maybe Char)
peekSymbol = get >>= \(_, remainBuff, _) ->
  case L.uncons remainBuff of
    Just (symbol, _) -> return $ Just symbol
    Nothing          -> return Nothing

scanConstant :: LexerMonad Token
scanConstant = peekSymbol >>= \case
  Nothing -> token
  Just s | isBoundary s -> token
         | isDigit    s -> consumeSymbol >> scanConstant
         | otherwise    -> scanUnknownToken
  where token :: LexerMonad Token
        token = do (_, _, lexeme) <- get
                   case T.decimal lexeme of
                      Right (value, _) -> return $ Constant value
                      Left  err        -> throwError $ MalformedConstant err

scanIdentifier :: LexerMonad Token
scanIdentifier = peekSymbol >>= \case
  Nothing -> token
  Just s | isAlphaNum_ s -> consumeSymbol >> scanIdentifier
         | otherwise -> token
  where token :: LexerMonad Token
        token = do (_, _, lexeme) <- get
                   case parseKeyword lexeme of
                     Just keyword -> return $ Keyword keyword
                     Nothing      -> return $ Identifier lexeme

scanCompoundToken :: [(String, Token)] -> LexerMonad Token
scanCompoundToken [] = throwError MalformedToken
scanCompoundToken tokens =
  do peekSymbol >>= \case
       Nothing -> throwError MalformedToken
       Just s  -> catchST ret $ consumeSymbol >> scanCompoundToken tokens'
         where scanCompoundToken' :: [(String, Token)] -> (Maybe Token, [(String, Token)])
               scanCompoundToken' [] = (Nothing, [])
               scanCompoundToken' (("", _) : ts) = scanCompoundToken' ts
               scanCompoundToken' ((c : "", t) : ts)
                 | c == s = (Just t, ts')
                 | otherwise = (t', ts')
                 where (t', ts') = scanCompoundToken' ts
               scanCompoundToken' ((c : cs, t) : ts)
                 | c == s = (t', (cs, t) : ts')
                 | otherwise = (t', ts')
                 where (t', ts') = scanCompoundToken' ts
               (token', tokens') = scanCompoundToken' tokens

               ret :: LexerError -> LexerMonad Token
               ret e = case token' of
                         Nothing -> throwError e
                         Just t  -> consumeSymbol >> return t

scanUnknownToken :: LexerMonad Token
scanUnknownToken = peekSymbol >>= \case
  Nothing -> unknownToken
  Just s | isBoundary s -> unknownToken
         | otherwise    -> consumeSymbol >> scanUnknownToken

unknownToken :: LexerMonad Token
unknownToken = do (line, col) <- getLexemeLineColumn
                  (_, _, lexeme) <- get
                  throwError UnknownToken { unknownLexeme = lexeme
                                          , unknownLexemeLine = line
                                          , unknownLexemeColumn = col
                                          }

getLexemeLineColumn :: LexerMonad (Int64, Int64)
getLexemeLineColumn = do (readBuff, _, lexeme) <- get
                         let buffUntilLexeme = L.dropEnd (fromIntegral $ T.length lexeme) readBuff
                         let lexemeLine = 1 + L.count "\n" buffUntilLexeme
                         let lexemeColumn = 1 + L.length (L.takeWhileEnd (/= '\n') buffUntilLexeme)
                         return (lexemeLine, lexemeColumn)

isAtEnd :: LexerMonad Bool
isAtEnd = get >>= \(_, remainBuff, _) -> return $ L.null remainBuff

lookAhead :: Int64 -> LexerMonad (Maybe Char)
lookAhead i = get >>= \(_, remainBuff, _) -> if i >= L.length remainBuff then return Nothing
                                             else return . Just $ L.index remainBuff i

isAlpha_ :: Char -> Bool
isAlpha_ '_' = True
isAlpha_ c   = isAlpha c

isAlphaNum_ :: Char -> Bool
isAlphaNum_ '_' = True
isAlphaNum_ c   = isAlpha c || isDigit c

isBoundary :: Char -> Bool
isBoundary = not . isAlphaNum_

