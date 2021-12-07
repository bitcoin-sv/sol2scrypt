{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Transpiler
import Intermediate
import Scrypt
import Solidity

main :: IO ()
main = do
  let solidityCode1 = "uint256"
  (scryptCode1, _ :: TranspilePath TypeName IType' (Maybe Type)) <- transpile solidityCode1
  putStrLn $ "Transpile `" ++ solidityCode1 ++ "` to `" ++ scryptCode1 ++ "`"
  
  let solidityCode2 = "0x123a"
  (scryptCode2, _ :: TranspilePath Expression IExpr' (Maybe (Expr IExpr))) <- transpile solidityCode2
  putStrLn $ "Transpile `" ++ solidityCode2 ++ "` to `" ++ scryptCode2 ++ "`"


