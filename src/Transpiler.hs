{-# LANGUAGE ScopedTypeVariables #-}
module Transpiler where

import Solidity
import Scrypt
import Intermediate

transpile :: String -> IO String
transpile solProgram = do
  -- TODO: use solProgram

  let solidityCode1 = "uint256" 
  intermediate1 <- transformSolTypeName solidityCode1
  let scrypt1 = transformIntermediateType <$> intermediate1
  scryptCode1 <- generateScrypt scrypt1
  putStrLn $ "Transpile `" ++ scryptCode1 ++ "` to `" ++ scryptCode1 ++ "`"

  let solidityCode2 = "0x123a" 
  intermediate2 <- transformSolExpression solidityCode2
  let scrypt2 = transformIntermediateExpr <$> intermediate2
  scryptCode2 <- generateScrypt scrypt2
  putStrLn $ "Transpile `" ++ solidityCode2 ++ "` to `" ++ scryptCode2 ++ "`"

  return ""






