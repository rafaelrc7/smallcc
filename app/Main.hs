module Main where

import qualified Settings         as S

import           Control.Monad    (unless, when)
import           GHC.IO.Handle    (hClose, hGetLine)
import           System.Directory (doesFileExist, removeFile)
import           System.Exit      (ExitCode (..), die, exitFailure, exitSuccess)
import           System.FilePath  (dropExtension, replaceExtension,
                                   takeExtension)
import           System.Process   (CreateProcess (..), StdStream (..),
                                   createProcess, proc, shell, waitForProcess)

import           Lexer
import Parser

main :: IO ()
main = do
  settings <- S.parseCLIArgs

  let sourceFile = S.inputFile settings
  let targetStage = S.mode settings
  let keepIntermediateFiles = S.keepIntermediateFiles settings

  doesInputFileExist <- doesFileExist sourceFile
  unless doesInputFileExist $ die ("Input file '" ++ sourceFile ++ "' does not exist")
  unless (takeExtension sourceFile == ".c") $ die ("Input file '" ++ sourceFile ++ "' is not a C source file")

  (_, Just hout, _, ph) <- createProcess (shell "which gcc"){ std_in = NoStream, std_out = CreatePipe, std_err = NoStream }
  waitForProcess ph >>= \exitCode -> unless (exitCode == ExitSuccess) $ die "Missing gcc in PATH"

  gccPath <- hGetLine hout
  hClose hout

  let runGCC = run gccPath

  let preProcessedFile = replaceExtension sourceFile ".i"
  runGCC [ "-E", "-P", sourceFile, "-o", preProcessedFile ]
  when (targetStage == S.PreProcessorMode) exitSuccess

  lexer <- fromFile preProcessedFile
  let lexerResult = scanUntilEOF lexer
  case lexerResult of
    Left err -> do unless keepIntermediateFiles $ removeFile preProcessedFile
                   print err
                   exitFailure
    Right tokens -> when (targetStage == S.CompilerMode S.LexerMode) $
      do print tokens
         unless keepIntermediateFiles $ removeFile preProcessedFile
         exitSuccess

  let tokens = case lexerResult of Right ts -> ts
                                   Left _ -> []

  let parserResult = parseProgram tokens
  case parserResult of
    Left err -> do unless keepIntermediateFiles $ removeFile preProcessedFile
                   print err
                   exitFailure
    Right ast -> when (targetStage == S.CompilerMode S.ParserMode) $
      do print ast
         unless keepIntermediateFiles $ removeFile preProcessedFile
         exitSuccess

  let assemblyFile = replaceExtension sourceFile ".s"
  runGCC [ "-S", preProcessedFile, "-o", assemblyFile ]
  unless keepIntermediateFiles $ removeFile preProcessedFile
  when (targetStage == S.CompilerMode S.CodeEmitterMode) exitSuccess

  let objectFile = replaceExtension sourceFile ".o"
  runGCC [ "-c", assemblyFile, "-o", objectFile ]
  unless keepIntermediateFiles $ removeFile assemblyFile
  when (targetStage == S.AssemblerMode) exitSuccess

  let executableFile = dropExtension sourceFile
  runGCC [ objectFile, "-o", executableFile ]
  unless keepIntermediateFiles $ removeFile objectFile

run :: String -> [String] -> IO ()
run cmd flags = do
  (_, _, _, ph) <- createProcess (proc cmd flags){ std_in = NoStream, std_out = NoStream }
  exitCode <- waitForProcess ph
  unless (exitCode == ExitSuccess) exitFailure

