{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

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

run :: Options -> IO ()
run (Transpile outputDir maybeSrc) = do
  let srcPath = fromMaybe "stdin" maybeSrc
  let baseName = takeBaseName srcPath
  result <- transpileFile srcPath
  let scryptFile = outputDir </> baseName ++ scryptExtension
  writeFile scryptFile result
  putStrLn $ "transpile result written to `" ++ scryptFile ++ "`"
    
run Cli.Version = putStrLn $ "Version: " ++ showVersion version ++ "+commit." ++  take 7 $(gitHash)
