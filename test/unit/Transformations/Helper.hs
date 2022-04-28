{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transformations.Helper where

import qualified Data.Map.Lazy as Map
import IR
import Solidity.Spec as Sol
import qualified Data.Set as Set

sol2Stmt :: String -> IO (Sol.Statement SourceRange)
sol2Stmt solidityCode = do
    e :: (Sol.Statement SourceRange) <- parseIO solidityCode ""
    return e

sol2Expr :: String -> IO (Expression SourceRange)
sol2Expr solidityCode = do
    e :: (Expression SourceRange) <- parseIO solidityCode ""
    return e

sol2Type :: String -> IO (TypeName SourceRange)
sol2Type solidityCode = do
    e :: (TypeName SourceRange) <- parseIO solidityCode ""
    return e

sol2Identifier :: String -> IO (Identifier SourceRange)
sol2Identifier solidityCode = do
    e :: (Identifier SourceRange) <- parseIO solidityCode ""
    return e

sol2Parameter :: String -> IO (Parameter SourceRange)
sol2Parameter solidityCode = do
    e :: (Parameter SourceRange) <- parseIO solidityCode ""
    return e

sol2StateVariable :: String -> IO (Sol.StateVariableDeclaration SourceRange)
sol2StateVariable solidityCode = do
    e :: (Sol.StateVariableDeclaration SourceRange) <- parseIO solidityCode ""
    return e

sol2ContractPart :: String -> IO (Sol.ContractPart SourceRange)
sol2ContractPart solidityCode = do
    e :: (Sol.ContractPart SourceRange) <- parseIO solidityCode ""
    return e

sol2Contract :: String -> IO (Sol.ContractDefinition SourceRange)
sol2Contract solidityCode = do
    e :: (Sol.ContractDefinition SourceRange) <- parseIO solidityCode ""
    return e

sol2Program :: String -> IO (SolidityCode SourceRange)
sol2Program solidityCode = do
    e :: (SolidityCode SourceRange) <- parseIO solidityCode ""
    return e

sol2Ir :: ToIRTransformable sol b => (String -> IO sol) -> String -> IO b
sol2Ir = sol2Ir' (TransformState [] Nothing Map.empty [] Map.empty [] 0 [] False Set.empty Set.empty [] False False)

sol2Ir' :: ToIRTransformable sol b => TransformState -> (String -> IO sol) -> String -> IO b
sol2Ir' initState f solidityCode = do
    ast <- f solidityCode
    (ir, _) <- transform2IR initState ast
    return ir
