{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transpiler where

import Intermediate
import Scrypt
import Solidity

-- Transpile Type Path: Solidity Type -> Intermediate Type -> Scrypt Type 
-- Used in function `transpile` to provide types information
data TranspilePath sol intm scr = TranspilePath sol intm scr

transpile :: (Parseable a, ToIntermediateTransformable a b, ToScryptTransformable b c, Generable c) => String -> IO (String, TranspilePath a b c)
transpile solidityCode = do
  sol :: a <- parseIO solidityCode
  itmd :: b <- transform2Intermediate TransformState sol
  scr :: c <- transform2Scrypt itmd
  scryptCode <- generateScrypt scr
  -- the purpose of return the second item (i.e. TranspilePath) is to provide type value of a, b, c to Haskell compiler
  return (scryptCode, TranspilePath sol itmd scr)
