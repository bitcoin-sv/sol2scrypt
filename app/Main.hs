{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Transpiler
import Cli
import Data.Version (showVersion)
import Control.Monad
import Data.Maybe
import Options.Applicative
import Development.GitRev
import Paths_sol2scrypt (version)
import System.FilePath (takeBaseName, (</>))

scryptExtension :: String
scryptExtension = ".scrypt"

main :: IO ()
main = run =<< execParser cli

validateOptions :: Options -> IO Bool
validateOptions (Transpile _ _) = do
    return True
validateOptions _ = return True

run :: Options -> IO ()
run options@(Transpile outputDir maybeSrc) = do
  valid <- validateOptions options
  when valid $ do
    let srcPath = fromMaybe "stdin" maybeSrc
    let baseName = takeBaseName srcPath
    results <- transpileFile srcPath
    let scryptFile = outputDir </> baseName ++ scryptExtension
    writeFile scryptFile results
    putStrLn $ "transpile results written to `" ++ scryptFile ++ "`"
run TranspileVersion = putStrLn $ "Version: " ++ showVersion version ++ "+commit." ++  take 7 $(gitHash)


