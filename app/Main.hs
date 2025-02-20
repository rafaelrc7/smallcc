module Main where

import           AssemblyEmitter
import           AssemblyGenerator
import           Control.Monad     (unless, when)
import           Data.Char         (isSpace)
import           Data.Functor      ((<&>))
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import           Error
import           Lexer
import           Parser
import           Pretty
import           SemanticAnalyzer
import qualified Settings          as S
import           System.Directory  (doesFileExist, removeFile)
import           System.Exit       (ExitCode (..), die, exitFailure,
                                    exitSuccess)
import           System.FilePath   (dropExtension, replaceExtension,
                                    takeExtension)
import           System.Process    (proc, readCreateProcessWithExitCode, shell)
import           Tacky

main :: IO ()
main = do
  settings <- S.parseCLIArgs

  let sourceFile = S.inputFile settings
  let targetStage = S.mode settings
  let keepIntermediateFiles = S.keepIntermediateFiles settings

  let cleanup file = unless keepIntermediateFiles $ removeFile file

  doesFileExist sourceFile >>= flip unless (die ("Input file '" ++ sourceFile ++ "' does not exist"))
  unless (takeExtension sourceFile == ".c") $ die ("Input file '" ++ sourceFile ++ "' is not a C source file")

  gccPath <- do
    (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (shell "which gcc") ""
    if exitCode == ExitSuccess
      then return $ takeWhile (not . isSpace) stdout
      else die stderr

  let runGCC = run gccPath

  -- Preprocessor
  let preProcessedFile = replaceExtension sourceFile ".i"
  runGCC ["-E", sourceFile, "-o", preProcessedFile]
  when (targetStage == S.PreProcessorMode) exitSuccess

  -- C Compiler
  -- Lexer
  lexerResult <- fromFile preProcessedFile <&> scanUntilEOF (T.pack preProcessedFile)
  cleanup preProcessedFile
  tokens <- case lexerResult of
    Left err -> do
      putError err
      exitFailure
    Right tokens -> do
      when (targetStage == S.CompilerMode S.LexerMode) $
        do
          print tokens
          exitSuccess
      return tokens

  -- Parser
  ast <- case parseProgram tokens of
    Left err -> do
      putError err
      exitFailure
    Right ast -> do
      when (targetStage == S.CompilerMode S.ParserMode) $
        do
          TIO.putStrLn $ pretty ast
          exitSuccess
      return ast

  -- Semantic Analyzer
  ast' <- case semanticAnalyze ast of
    Left err -> do
      print err
      exitFailure
    Right ast' -> do
      when (targetStage == S.CompilerMode S.SemanticAnalyzerMode) $
        do
          TIO.putStrLn $ pretty ast'
          exitSuccess
      return ast'

  -- Tacky generator
  let tackyAst = translateTacky ast'
  when (targetStage == S.CompilerMode S.TackyMode) $
    do
      print tackyAst
      exitSuccess

  -- Code Generator
  let assemblyAst = translate tackyAst
  when (targetStage == S.CompilerMode S.CodeGeneratorMode) $
    do
      print assemblyAst
      exitSuccess

  -- Code Emitter
  let assemblyFile = replaceExtension sourceFile ".s"
  TIO.writeFile assemblyFile $ emitAssembly assemblyAst
  when (targetStage == S.CompilerMode S.CodeEmitterMode) exitSuccess

  let objectFile = replaceExtension sourceFile ".o"
  runGCC ["-c", assemblyFile, "-o", objectFile]
  cleanup assemblyFile
  when (targetStage == S.AssemblerMode) exitSuccess

  let executableFile = dropExtension sourceFile
  runGCC [objectFile, "-o", executableFile]
  cleanup objectFile

run :: String -> [String] -> IO ()
run cmd flags = do
  (exitCode, _, stderr) <- readCreateProcessWithExitCode (proc cmd flags) ""
  unless (exitCode == ExitSuccess) $ die stderr
