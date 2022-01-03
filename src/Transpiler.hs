{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transpiler where

import IR
import Scrypt
import Solidity

-- Transpile Type Path: Solidity Type -> IR Type -> Scrypt Type 
-- Used in function `transpile` to provide types information
data TranspilePath sol ir scr = TranspilePath sol ir scr

data TranspileResult a b c = TranspileResult {scryptCode :: String, tranpilePath :: TranspilePath a b c}

transpile :: (Parseable a, ToIRTransformable a b, ToScryptTransformable b c, Generable c) => String -> IO (TranspileResult a b c)
transpile solidityCode = do
  sol :: a <- parseIO solidityCode
  itmd :: b <- transform2IR (TransformState []) sol
  scr :: c <- transform2Scrypt itmd
  code <- generateScrypt (CodeGenState 0) scr
  -- the purpose of including `TranspilePath` is to provide the type values of `a`, `b`, `c` to Haskell compiler
  return $ TranspileResult code $ TranspilePath sol itmd scr
