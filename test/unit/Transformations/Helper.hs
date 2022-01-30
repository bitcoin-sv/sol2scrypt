{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transformations.Helper where

import IR
import Transpiler 
import Utils
import Solidity.Spec as Sol
import Scrypt.Spec as Scr
import Scrypt.Generables.Base


sol2Stmt :: String -> IO Sol.Statement
sol2Stmt solidityCode = do
    e :: Sol.Statement <- parseIO solidityCode
    return e

sol2Expr :: String -> IO Expression
sol2Expr solidityCode = do
    e :: Sol.Expression <- parseIO solidityCode
    return e

sol2Type :: String -> IO TypeName
sol2Type solidityCode = do
    e :: Sol.TypeName <- parseIO solidityCode
    return e

sol2Identifier :: String -> IO Identifier
sol2Identifier solidityCode = do
    e :: Sol.Identifier <- parseIO solidityCode
    return e

sol2Parameter :: String -> IO Parameter
sol2Parameter solidityCode = do
    e :: Sol.Parameter <- parseIO solidityCode
    return e

sol2StateVariable :: String -> IO StateVariableDeclaration
sol2StateVariable solidityCode = do
    e :: Sol.StateVariableDeclaration <- parseIO solidityCode
    return e

sol2ContractPart :: String -> IO ContractPart
sol2ContractPart solidityCode = do
    e :: Sol.ContractPart <- parseIO solidityCode
    return e

sol2Contract :: String -> IO ContractDefinition
sol2Contract solidityCode = do
    e :: Sol.ContractDefinition <- parseIO solidityCode
    return e

sol2Program :: String -> IO SolidityCode
sol2Program solidityCode = do
    e :: Sol.SolidityCode <- parseIO solidityCode
    return e

sol2Ir :: ToIRTransformable sol b => (String -> IO sol) -> String -> IO b
sol2Ir f solidityCode = do
    ast <- f solidityCode
    transform2IR (TransformState $ GState [] $ Returned False) ast


-- sol2scr :: (ToIRTransformable sol ir, ToScryptTransformable ir scr) => (String -> IO sol) -> String -> IO b
-- sol2scr f solidityCode = do
--     ir' :: ir <- sol2Ir f solidityCode
--     scr :: scr <- transform2Scrypt ir'
--     return scr