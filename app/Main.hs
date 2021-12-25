{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import IR
import Scrypt as Scr
import Solidity as Sol
import Transpiler
import Utils


main :: IO ()
main = do

  let solidityCode1 = "uint256"
  tr1 :: TranspileResult TypeName IType' (Maybe Type) <- transpile solidityCode1
  putStrLn $ "Transpile `" ++ solidityCode1 ++ "` to `" ++ scryptCode tr1 ++ "`"

  let solidityCode2 = "0x123a"
  tr2 :: TranspileResult Expression IExpr' (Maybe (Expr Ann)) <- transpile solidityCode2
  putStrLn $ "Transpile `" ++ solidityCode2 ++ "` to `" ++ scryptCode tr2 ++ "`"

  let solidityCode = "function set(uint x) external { x; }"
  f :: ContractPart <- parseIO solidityCode
  print f

  let solidityCode3 = "function set(uint x) external { 1; }"
  tr3 :: TranspileResult ContractPart IFunction' (Maybe (Scr.Function Ann)) <- transpile solidityCode3
  putStrLn $ "Transpile `" ++ solidityCode3 ++ "` to `" ++ scryptCode tr3 ++ "`"
