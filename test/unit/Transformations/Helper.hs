{-# LANGUAGE ScopedTypeVariables #-}

module Transformations.Helper where

import IR
import Scrypt
import Solidity
import Transpiler
import Utils
import qualified Solidity.Spec as Sol



sol2Stmt solidityCode = do
    e :: Sol.Statement <- parseIO solidityCode
    return e

sol2Expr solidityCode = do
    e :: Sol.Expression <- parseIO solidityCode
    return e

sol2Type solidityCode = do
    e :: Sol.TypeName <- parseIO solidityCode
    return e

sol2Parameter solidityCode = do
    e :: Sol.Parameter <- parseIO solidityCode
    return e

sol2Ir f solidityCode = do 
    ast <- f solidityCode
    ir <- transform2IR TransformState ast
    return ir