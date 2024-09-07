{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer.Scanner where

import           Lexer.Error          (LexerError (..), LexerErrorType (..))
import           Lexer.Token          (Lexeme, Location (..),
                                       ScannedSymbol (..), Token (..),
                                       TokenType (..), scanKeyword)

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
  EOF -> err UnexpectedSymbol {got=EOF, expected=s}
  Symbol s' | s' == s -> return ()
            | otherwise -> err UnexpectedSymbol {got=Symbol s', expected=s}

nextCol :: Location -> Location
nextCol loc@Location { lexemeColumn = c } = loc { lexemeColumn = succ c }

nextLine :: Location -> Location
nextLine loc@Location { lexemeLine = l } = loc { lexemeLine = succ l, lexemeColumn = 1 }

nextToken :: LexerMonad Token
nextToken = modify resetLexeme >> scanToken

scanToken :: LexerMonad Token
scanToken = peekSymbol >>= \case
  EOF        -> err ReachedEOF
  Symbol '"' -> scanString
  Symbol '#' -> scanLineMarker >> nextToken
  Symbol '(' -> consumeSymbol  >> ret OpenParens
  Symbol ')' -> consumeSymbol  >> ret CloseParens
  Symbol '{' -> consumeSymbol  >> ret OpenBrace
  Symbol '}' -> consumeSymbol  >> ret CloseBrace
  Symbol ';' -> consumeSymbol  >> ret Semicolon
  Symbol '~' -> consumeSymbol  >> ret Complement
  Symbol s | isSpace  s -> consumeSymbol >> nextToken
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

setLine :: Int64 -> LexerMonad ()
setLine lin = get >>= \(LexerState (RemainingBuffer b loc) l) ->
  put $ LexerState (RemainingBuffer b loc {lexemeLine=lin}) l

setBufferName :: T.Text -> LexerMonad ()
setBufferName buffName = get >>= \(LexerState (RemainingBuffer b loc) l) ->
  put $ LexerState (RemainingBuffer b loc {lexemeBuffer=buffName}) l

scanLineMarker :: LexerMonad ()
scanLineMarker = do (LexerState (RemainingBuffer _ Location {lexemeColumn=c}) _) <- get
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

                    setLine (fromIntegral line)
                    setBufferName buff

                    consumeUntilEOL


consumeUntilEOL :: LexerMonad ()
consumeUntilEOL = consumeSymbol >>= \case
                    (Symbol '\n') -> return ()
                    EOF           -> return ()
                    _             -> consumeUntilEOL

consumeSymbol :: LexerMonad ScannedSymbol
consumeSymbol = get >>= \(LexerState (RemainingBuffer b bloc) (CurrentLexeme l lloc)) ->
                  case L.uncons b of
                    Nothing -> return EOF
                    Just (s@'\n', b') ->
                      do put (LexerState (RemainingBuffer b' (nextLine bloc)) (CurrentLexeme (l `T.snoc` s) lloc))
                         return $ Symbol s
                    Just (s, b') ->
                      do put (LexerState (RemainingBuffer b' (nextCol bloc))  (CurrentLexeme (l `T.snoc` s) lloc))
                         return $ Symbol s

peekSymbol :: LexerMonad ScannedSymbol
peekSymbol = get >>= \(LexerState (RemainingBuffer b _) _) ->
               case L.uncons b of
                 Just (symbol, _) -> return $ Symbol symbol
                 Nothing          -> return EOF

scanConstant :: LexerMonad Token
scanConstant = peekSymbol >>= \case
  EOF -> token
  Symbol s | isBoundary s -> token
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
          \case EOF         -> err MalformedToken
                Symbol '\\' -> T.cons <$> scanEscaped <*> scanString'
                Symbol '"'  -> consumeSymbol >> return ""
                Symbol c    -> consumeSymbol >> scanString' <&> T.cons c

scanEscaped :: LexerMonad Char
scanEscaped = expect '\\' >> consumeSymbol >>=
  \case Symbol 'a'  -> return '\a'
        Symbol 'b'  -> return '\b'
        Symbol 'f'  -> return '\f'
        Symbol 'n'  -> return '\n'
        Symbol 'r'  -> return '\r'
        Symbol 't'  -> return '\t'
        Symbol 'v'  -> return '\v'
        Symbol '\\' -> return '\\'
        Symbol '\'' -> return '\''
        Symbol '\"' -> return '\"'
        Symbol '?'  -> return '?'
        _           -> err MalformedToken

scanIdentifier :: LexerMonad Token
scanIdentifier = peekSymbol >>= \case
  EOF -> token
  Symbol s | isAlphaNum_ s -> consumeSymbol >> scanIdentifier
           | otherwise -> token
  where token :: LexerMonad Token
        token = get >>= \(LexerState _ (CurrentLexeme l _)) ->
                  case scanKeyword l of
                    Just kw -> ret $ Keyword kw
                    Nothing -> ret $ Identifier l

scanCompoundToken :: [(String, TokenType)] -> LexerMonad Token
scanCompoundToken [] = err MalformedToken
scanCompoundToken tokens = peekSymbol >>= \case
   EOF      -> err MalformedToken
   Symbol s -> catchST token $ consumeSymbol >> scanCompoundToken tokens'
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
  EOF -> err UnknownToken
  Symbol s | isBoundary s -> err UnknownToken
           | otherwise    -> consumeSymbol >> scanUnknownToken

isAlpha_ :: Char -> Bool
isAlpha_ '_' = True
isAlpha_ c   = isAlpha c

isAlphaNum_ :: Char -> Bool
isAlphaNum_ '_' = True
isAlphaNum_ c   = isAlpha c || isDigit c

isBoundary :: Char -> Bool
isBoundary = not . isAlphaNum_
