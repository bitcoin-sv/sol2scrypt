{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Aeson hiding (Options)

scryptExtension :: String
scryptExtension = ".scrypt"

main :: IO ()
main = run =<< execParser cli

run :: Options -> IO ()
run (Transpile maybeOutputDir maybeSrc forceOutput) = do
  let srcPath = fromMaybe "stdin" maybeSrc
  let baseName = takeBaseName srcPath
  result <- transpileFile srcPath
  let scryptFile = case maybeOutputDir of
                      Just dir -> Just $ dir </> baseName ++ scryptExtension
                      _ -> Nothing
      logs = transpileLogs result
  if isJust scryptFile
    then do
      -- output logs to console
      outputLog logs
      -- output scrypt to file
      when (willOutputScrypt logs forceOutput) $ do
        let scryptFile' = fromJust scryptFile
        writeFile scryptFile' $ scryptCode result
        putStrLn $ "transpile result written to `" ++ scryptFile' ++ "`"
    else do
      -- output logs & scrypt to stdout in json format
      putStrLn $ jsonPrettyPrint $ StdOutput logs (scryptCode result) forceOutput
  
run Cli.Version = putStrLn $ "Version: " ++ showVersion version ++ "+commit." ++ take 7 $(gitHash)

outputLog :: Logs -> IO ()
outputLog = mapM_ toConsole
  where
    toConsole l = do
      putStrLn $ show (logLevel l) ++ ": " ++ serializeSourceRange (logSrc l)
      putStrLn $ logMessage l
      putStrLn ""

gotTranspileError :: Logs -> Bool
gotTranspileError = foldl (\r l -> r || logLevel l == ErrorLevel) False

willOutputScrypt :: Logs -> Bool -> Bool
willOutputScrypt logs forceOutput = forceOutput || not (gotTranspileError logs)

data StdOutput = StdOutput Logs String Bool deriving (Show)

instance ToJSON StdOutput where
  toJSON (StdOutput logs code forceOutput) = object $ ["scrypt" .= code | willOutputScrypt logs forceOutput] ++ ["logs" .= toJSON logs]