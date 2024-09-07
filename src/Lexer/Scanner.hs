{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer.Scanner where

import Lexer.Token ( Lexeme, Token(..), TokenType(..), Location(..), scanKeyword )
import Lexer.Error ( LexerError(..), LexerErrorType(..) )

import           Control.Monad.Except (ExceptT, MonadError (throwError),
                                       handleError)
import           Control.Monad.State  (MonadState (get, put), State, modify)
import           Data.Char            (isAlpha, isDigit, isSpace)
import           Data.Int             (Int64)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as L
import qualified Data.Text.Read       as T

type Buffer = L.Text

data CurrentLexeme = CurrentLexeme Lexeme Location
  deriving Show

data RemainingBuffer = RemainingBuffer Buffer Location
  deriving Show

data LexerState = LexerState RemainingBuffer CurrentLexeme
  deriving Show

type LexerMonad a = ExceptT LexerError (State LexerState) a -- WriterT ?

resetLexeme :: LexerState -> LexerState
resetLexeme (LexerState rb@(RemainingBuffer _ loc) _) = LexerState rb (CurrentLexeme T.empty loc)

catchST :: (LexerError -> LexerMonad a) -> LexerMonad a -> LexerMonad a
catchST handler action = get >>= \st -> handleError (\e -> put st >> handler e) action

ret :: TokenType -> LexerMonad Token
ret tok = get >>= \(LexerState _ (CurrentLexeme l lloc)) -> return $ Token tok l lloc

err :: LexerErrorType -> LexerMonad a
err e = get >>= \(LexerState _ (CurrentLexeme l lloc)) -> throwError $ LexerError e l lloc

nextCol :: Location -> Location
nextCol loc@Location { lexemeColumn = c } = loc { lexemeColumn = succ c }

nextLine :: Location -> Location
nextLine loc@Location { lexemeLine = l } = loc { lexemeLine = succ l, lexemeColumn = 1 }

nextToken :: LexerMonad Token
nextToken = modify resetLexeme >> scanToken

scanToken :: LexerMonad Token
scanToken = peekSymbol >>= \case
  Nothing  -> err EOF
  Just '(' -> consumeSymbol >> ret OpenParens
  Just ')' -> consumeSymbol >> ret CloseParens
  Just '{' -> consumeSymbol >> ret OpenBrace
  Just '}' -> consumeSymbol >> ret CloseBrace
  Just ';' -> consumeSymbol >> ret Semicolon
  Just '~' -> consumeSymbol >> ret Complement
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
consumeSymbol = do (LexerState (RemainingBuffer b bloc) (CurrentLexeme l lloc)) <- get
                   case L.uncons b of
                     Nothing -> return Nothing
                     Just (s@'\n', b') ->
                       do put (LexerState (RemainingBuffer b' (nextLine bloc)) (CurrentLexeme (l `T.snoc` s) lloc))
                          return $ Just s
                     Just (s, b') ->
                       do put (LexerState (RemainingBuffer b' (nextCol bloc))  (CurrentLexeme (l `T.snoc` s) lloc))
                          return $ Just s

peekSymbol :: LexerMonad (Maybe Char)
peekSymbol = do (LexerState (RemainingBuffer b _) _) <- get
                case L.uncons b of
                  Just (symbol, _) -> return $ Just symbol
                  Nothing          -> return Nothing

scanConstant :: LexerMonad Token
scanConstant = peekSymbol >>= \case
  Nothing -> token
  Just s | isBoundary s -> token
         | isDigit    s -> consumeSymbol >> scanConstant
         | otherwise    -> scanUnknownToken
  where token :: LexerMonad Token
        token = do (LexerState (RemainingBuffer _ _) (CurrentLexeme l _)) <- get
                   case T.decimal l of
                      Right (value, _) -> ret $ Constant value
                      Left  e          -> err $ MalformedConstant e

scanIdentifier :: LexerMonad Token
scanIdentifier = peekSymbol >>= \case
  Nothing -> token
  Just s | isAlphaNum_ s -> consumeSymbol >> scanIdentifier
         | otherwise -> token
  where token :: LexerMonad Token
        token = do (LexerState _ (CurrentLexeme l _)) <- get
                   case scanKeyword l of
                     Just kw -> ret $ Keyword kw
                     Nothing -> ret $ Identifier l

scanCompoundToken :: [(String, TokenType)] -> LexerMonad Token
scanCompoundToken [] = err MalformedToken
scanCompoundToken tokens =
  do peekSymbol >>= \case
       Nothing -> err MalformedToken
       Just s  -> catchST token $ consumeSymbol >> scanCompoundToken tokens'
         where scanCompoundToken' :: [(String, TokenType)] -> (Maybe TokenType, [(String, TokenType)])
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

               token :: LexerError -> LexerMonad Token
               token e = case token' of
                         Nothing -> throwError e
                         Just t  -> consumeSymbol >> ret t

scanUnknownToken :: LexerMonad Token
scanUnknownToken = peekSymbol >>= \case
  Nothing -> err UnknownToken
  Just s | isBoundary s -> err UnknownToken
         | otherwise    -> consumeSymbol >> scanUnknownToken

isAtEnd :: LexerMonad Bool
isAtEnd = do (LexerState (RemainingBuffer b _) _) <- get
             return $ L.null b

lookAhead :: Int64 -> LexerMonad (Maybe Char)
lookAhead i = do (LexerState (RemainingBuffer b _) _) <- get
                 if i >= L.length b then
                   return Nothing
                 else
                   return . Just $ L.index b i

isAlpha_ :: Char -> Bool
isAlpha_ '_' = True
isAlpha_ c   = isAlpha c

isAlphaNum_ :: Char -> Bool
isAlphaNum_ '_' = True
isAlphaNum_ c   = isAlpha c || isDigit c

isBoundary :: Char -> Bool
isBoundary = not . isAlphaNum_
