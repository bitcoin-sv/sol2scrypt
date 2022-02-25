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
import Solidity.Spec

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
    
run Version = putStrLn $ "Version: " ++ showVersion version ++ "+commit." ++  take 7 $(gitHash)





runTest = do

  let solidityCode1 = "uint256"
  tr1 :: TranspileResult (TypeName' SourceRange) IType' (Maybe Type) <- transpile solidityCode1
  putStrLn $ "Transpile `" ++ solidityCode1 ++ "` to `" ++ scryptCode tr1 ++ "`"

  let solidityCode2 = "0x123a"
  tr2 :: TranspileResult Expression IExpression' (Maybe (Expr Ann)) <- transpile solidityCode2
  putStrLn $ "Transpile `" ++ solidityCode2 ++ "` to `" ++ scryptCode tr2 ++ "`"


  let solidityCode = "function set(uint x) external { x; }"
  f :: ContractPart <- parseIO solidityCode
  print f

  let solidityCode3 = "function set(uint x) external { 1; }"
  tr3 :: TranspileResult ContractPart IFunction' (Maybe (Scr.Function Ann)) <- transpile solidityCode3
  putStrLn $ "Transpile `" ++ solidityCode3 ++ "` to `" ++ scryptCode tr3 ++ "`"



  let solidityCode4 = "uint256[2] storedData;"
  e :: TypeName' SourceRange <- parseIO solidityCode4
  print e

