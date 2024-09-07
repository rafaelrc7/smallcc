{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer.Scanner where

import           Lexer.Error          (LexerError (..), LexerErrorType (..))
import           Lexer.Token          (Lexeme, Location (..), Token (..),
                                       TokenType (..), scanKeyword)

import           Control.Monad        (void, when)
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
  Just '#' -> scanLineMarker >> nextToken
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

setLine :: Int64 -> LexerMonad ()
setLine lin = get >>= \(LexerState (RemainingBuffer b Location {lexemeLine=_, lexemeColumn=c}) l) ->
  put $ LexerState (RemainingBuffer b Location {lexemeLine=lin, lexemeColumn=c}) l

scanLineMarker :: LexerMonad ()
scanLineMarker = do (LexerState (RemainingBuffer _ Location {lexemeColumn=c}) _) <- get
                    _ <- consumeSymbol -- #
                    when (c /= 1) $ void scanUnknownToken
                    _ <- consumeSymbol -- <space>
                    modify resetLexeme
                    scanConstant >>=
                      \case Token (Constant l) _ _ -> consumeUntilEOL >> setLine (fromIntegral l)
                            _ -> consumeUntilEOL
  where consumeUntilEOL :: LexerMonad ()
        consumeUntilEOL = consumeSymbol >>= \case
                            Just '\n' -> return ()
                            Nothing   -> return ()
                            _         -> consumeUntilEOL

consumeSymbol :: LexerMonad (Maybe Char)
consumeSymbol = get >>= \(LexerState (RemainingBuffer b bloc) (CurrentLexeme l lloc)) ->
                  case L.uncons b of
                    Nothing -> return Nothing
                    Just (s@'\n', b') ->
                      do put (LexerState (RemainingBuffer b' (nextLine bloc)) (CurrentLexeme (l `T.snoc` s) lloc))
                         return $ Just s
                    Just (s, b') ->
                      do put (LexerState (RemainingBuffer b' (nextCol bloc))  (CurrentLexeme (l `T.snoc` s) lloc))
                         return $ Just s

peekSymbol :: LexerMonad (Maybe Char)
peekSymbol = get >>= \(LexerState (RemainingBuffer b _) _) ->
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
        token = get >>= \(LexerState (RemainingBuffer _ _) (CurrentLexeme l _)) ->
                  case T.decimal l of
                     Right (value, _) -> ret $ Constant value
                     Left  e          -> err $ MalformedConstant e

scanIdentifier :: LexerMonad Token
scanIdentifier = peekSymbol >>= \case
  Nothing -> token
  Just s | isAlphaNum_ s -> consumeSymbol >> scanIdentifier
         | otherwise -> token
  where token :: LexerMonad Token
        token = get >>= \(LexerState _ (CurrentLexeme l _)) ->
                  case scanKeyword l of
                    Just kw -> ret $ Keyword kw
                    Nothing -> ret $ Identifier l

scanCompoundToken :: [(String, TokenType)] -> LexerMonad Token
scanCompoundToken [] = err MalformedToken
scanCompoundToken tokens = peekSymbol >>= \case
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
isAtEnd = get >>= \(LexerState (RemainingBuffer b _) _) ->
            return $ L.null b

lookAhead :: Int64 -> LexerMonad (Maybe Char)
lookAhead i = get >>= \(LexerState (RemainingBuffer b _) _) ->
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
