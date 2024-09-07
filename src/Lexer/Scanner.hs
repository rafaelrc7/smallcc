{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer.Scanner where

import           Lexer.Error          (LexerError (..), LexerErrorType (..))
import           Lexer.Token          (Lexeme, Token (..), TokenType (..),
                                       scanKeyword)
import           Location

import           Control.Monad        (void, when)
import           Control.Monad.Except (ExceptT, MonadError (throwError),
                                       handleError)
import           Control.Monad.State  (MonadState (get, put), State, modify)
import           Data.Char            (isAlpha, isDigit, isSpace)
import           Data.Functor         ((<&>))
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

expect :: Char -> LexerMonad ()
expect s = consumeSymbol >>= \case
  Nothing -> err $ UnexpectedSymbol s Nothing
  Just s' | s' == s -> return ()
          | otherwise -> err $ UnexpectedSymbol s (Just s')

nextCol :: Location -> Location
nextCol (Location line col buff) = Location line (succ col) buff

nextLine :: Location -> Location
nextLine (Location line _ buff) = Location (succ line) 1 buff

setLine :: Location -> Line -> Location
setLine (Location _ col buff) line = Location line col buff

setBufferName :: Location -> T.Text -> Location
setBufferName (Location line col _) buff = Location line col (Just buff)

nextToken :: LexerMonad Token
nextToken = modify resetLexeme >> scanToken

scanToken :: LexerMonad Token
scanToken = peekSymbol >>= \case
  Nothing  -> err ReachedEOF
  Just '"' -> scanString
  Just '#' -> scanLineMarker >> nextToken
  Just '(' -> consumeSymbol  >> ret OpenParens
  Just ')' -> consumeSymbol  >> ret CloseParens
  Just '{' -> consumeSymbol  >> ret OpenBrace
  Just '}' -> consumeSymbol  >> ret CloseBrace
  Just ';' -> consumeSymbol  >> ret Semicolon
  Just '~' -> consumeSymbol  >> ret Complement
  Just s | isSpace  s -> consumeSymbol >> nextToken
           | isDigit  s -> consumeSymbol >> scanConstant
           | isAlpha_ s -> consumeSymbol >> scanIdentifier
           | otherwise  -> scanCompoundToken
               [ ("=", Equals),       ("==", EqualsTo)
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

setLine' :: Int64 -> LexerMonad ()
setLine' line = modify (\(LexerState (RemainingBuffer buff loc) lexe) -> LexerState (RemainingBuffer buff (setLine loc line)) lexe)

setBufferName' :: T.Text -> LexerMonad ()
setBufferName' buffName = modify (\(LexerState (RemainingBuffer buff loc) lexe) -> LexerState (RemainingBuffer buff (setBufferName loc buffName)) lexe)

scanLineMarker :: LexerMonad ()
scanLineMarker = do (LexerState (RemainingBuffer _ (Location _ c _)) _) <- get
                    expect '#'
                    when (c /= 1) $ void scanUnknownToken
                    expect ' '

                    modify resetLexeme
                    line <- scanConstant >>= \case
                      Token (Constant l) _ _ -> return l
                      _                      -> err MalformedToken

                    expect ' '
                    modify resetLexeme
                    buff <- scanString >>= \case
                      Token (String s) _ _ -> return s
                      _                    -> err MalformedToken

                    setLine' (fromIntegral line)
                    setBufferName' buff

                    consumeUntilEOL


consumeUntilEOL :: LexerMonad ()
consumeUntilEOL = consumeSymbol >>= \case
                    (Just '\n') -> return ()
                    Nothing     -> return ()
                    _           -> consumeUntilEOL

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

scanString :: LexerMonad Token
scanString = expect '"' >> scanString' >>= ret . String
  where scanString' :: LexerMonad T.Text
        scanString' = peekSymbol >>=
          \case Nothing   -> err MalformedToken
                Just '\\' -> T.cons <$> scanEscaped <*> scanString'
                Just '"'  -> consumeSymbol >> return ""
                Just c    -> consumeSymbol >> scanString' <&> T.cons c

scanEscaped :: LexerMonad Char
scanEscaped = expect '\\' >> consumeSymbol >>=
  \case Just 'a'  -> return '\a'
        Just 'b'  -> return '\b'
        Just 'f'  -> return '\f'
        Just 'n'  -> return '\n'
        Just 'r'  -> return '\r'
        Just 't'  -> return '\t'
        Just 'v'  -> return '\v'
        Just '\\' -> return '\\'
        Just '\'' -> return '\''
        Just '\"' -> return '\"'
        Just '?'  -> return '?'
        _         -> err MalformedToken

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

isAlpha_ :: Char -> Bool
isAlpha_ '_' = True
isAlpha_ c   = isAlpha c

isAlphaNum_ :: Char -> Bool
isAlphaNum_ '_' = True
isAlphaNum_ c   = isAlpha c || isDigit c

isBoundary :: Char -> Bool
isBoundary = not . isAlphaNum_
