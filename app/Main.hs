{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Cli
import Control.Monad.Except
import Data.Maybe
import Data.Version (showVersion)
import Development.GitRev
import IR.Transformations.Base
import Json
import Options.Applicative
import Paths_sol2scrypt (version)
import System.FilePath (takeBaseName, (</>))
import Transpiler

scryptExtension :: String
scryptExtension = ".scrypt"

main :: IO ()
main = run =<< execParser cli

run :: Options -> IO ()
run (Transpile outputDir maybeSrc logToFile forceOutput) = do
  let srcPath = fromMaybe "stdin" maybeSrc
  let baseName = takeBaseName srcPath
  result <- transpileFile srcPath
  let scryptFile = outputDir </> baseName ++ scryptExtension
  let logs = transpileLogs result
  outputLog logToFile (outputDir </> baseName) logs
  when (forceOutput || not (gotTranspileError logs)) $ do
    writeFile scryptFile $ scryptCode result
    putStrLn $ "transpile result written to `" ++ scryptFile ++ "`"
run Cli.Version = putStrLn $ "Version: " ++ showVersion version ++ "+commit." ++ take 7 $(gitHash)

outputLog :: Bool -> FilePath -> Logs -> IO ()
outputLog True fileName logs = do
  let logJson = jsonPrettyPrint logs
  writeFile (fileName ++ ".log.json") logJson
outputLog False _ logs = mapM_ toConsole logs
  where
    toConsole l = do
      putStrLn $ show (logLevel l) ++ ": " ++ serializeSourceRange (logSrc l)
      putStrLn $ logMessage l
      putStrLn ""

gotTranspileError :: Logs -> Bool
gotTranspileError = foldl (\r l -> r || logLevel l == ErrorLevel) False
