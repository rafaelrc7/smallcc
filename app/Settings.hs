module Settings ( CompilerMode(..)
                , Mode(..)
                , Settings(..)
                , parseCLIArgs
) where

import           Options.Applicative

data CompilerMode = LexerMode
                  | ParserMode
                  | Tacky
                  | CodeGeneratorMode
                  | CodeEmitterMode
 deriving (Show, Eq)

data Mode = PreProcessorMode
          | CompilerMode CompilerMode
          | AssemblerMode
          | LinkerMode
 deriving (Show, Eq)

data Settings = Settings
  { mode                  :: Mode
  , keepIntermediateFiles :: Bool
  , inputFile             :: FilePath
  }
 deriving (Show, Eq)

parseCLIArgs :: IO Settings
parseCLIArgs = execParser opts

opts :: ParserInfo Settings
opts = info (settingsParser <**> helper)
  (  fullDesc
  <> progDesc "Compile a C FILE"
  <> header "Small C Compiler" )

settingsParser :: Parser Settings
settingsParser = Settings <$> modeParser <*> keepIntermediateFilesParser <*> inputFileParser

modeParser :: Parser Mode
modeParser = preProcessorMode <|> lexerMode <|> parserMode <|> tackyMode
              <|> codeGeneratorMode <|> codeEmitterMode <|> assemblerMode
              <|> linkerMode <|> pure LinkerMode

keepIntermediateFilesParser :: Parser Bool
keepIntermediateFilesParser = switch ( long "keep" <> short 'K' <> help "Keep intermediate files" )

inputFileParser :: Parser FilePath
inputFileParser = argument str (metavar "FILE")

preProcessorMode :: Parser Mode
preProcessorMode = flag' PreProcessorMode
  (  long "preprocessor"
  <> short 'E'
  <> help "Run up to the pre processor" )

lexerMode :: Parser Mode
lexerMode = flag' (CompilerMode LexerMode)
  (  long "lex"
  <> help "Run up to the lexer" )

parserMode :: Parser Mode
parserMode = flag' (CompilerMode ParserMode)
  (  long "parse"
  <> help "Run up to the parser" )

tackyMode :: Parser Mode
tackyMode = flag' (CompilerMode Tacky)
  (  long "tacky"
  <> help "Run up to tacky generation" )

codeGeneratorMode :: Parser Mode
codeGeneratorMode = flag' (CompilerMode CodeGeneratorMode)
  (  long "codegen"
  <> help "Run up to code generation" )

codeEmitterMode :: Parser Mode
codeEmitterMode = flag' (CompilerMode CodeEmitterMode)
  (  long "codeemit"
  <> short 'S'
  <> help "Run up to code emission" )

assemblerMode :: Parser Mode
assemblerMode = flag' AssemblerMode
  (  long "assembler"
  <> short 'c'
  <> help "Run up to the assembler" )

linkerMode :: Parser Mode
linkerMode = flag' LinkerMode
  (  long "linker"
  <> help "Run up to the linker" )

