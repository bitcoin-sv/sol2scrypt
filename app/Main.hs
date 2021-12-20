{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import IR
import Scrypt
import Solidity
import Transpiler



main :: IO ()
main = do

  -- int
  let solidityCode1 = "uint256"

  tr1 :: TranspileResult TypeName IType' (Maybe Type) <- transpile solidityCode1
  putStrLn $ "Transpile `" ++ solidityCode1 ++ "` to `" ++ scryptCode tr1 ++ "`"

  let solidityCode2 = "0x123a"
  tr2 :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile solidityCode2
  putStrLn $ "Transpile `" ++ solidityCode2 ++ "` to `" ++ scryptCode tr2 ++ "`"
