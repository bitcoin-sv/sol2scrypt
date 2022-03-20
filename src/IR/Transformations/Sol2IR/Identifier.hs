{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Identifier where

import IR.Spec as IR
import IR.Transformations.Base
import Solidity.Spec as Sol

instance ToIRTransformable (Identifier SourceRange) IIdentifier' where
  _toIR (Sol.Identifier i _) = return $ Just $ IR.Identifier i

maybeStateVarId :: IR.IIdentifier' -> Transformation IR.IIdentifier'
maybeStateVarId Nothing = return Nothing
maybeStateVarId (Just i) = do
  s <- lookupSym i
  i' <- case s of
    -- append prefix `this.` for state variables
    Just (Symbol _ _ True False _) -> return $ IR.Identifier $ "this." ++ IR.unIdentifier i
    _ -> return i
  return $ Just i'

maybeMemberFunctionCall :: IR.IIdentifier' -> Transformation IR.IIdentifier'
maybeMemberFunctionCall Nothing = return Nothing
maybeMemberFunctionCall (Just i) = do
  s <- lookupSym i
  i' <- case s of
    -- append prefix `this.` for non-static functions
    Just (Symbol _ (BuiltinType "function") _ _ False) -> return $ IR.Identifier $ "this." ++ IR.unIdentifier i
    _ -> return i
  return $ Just i'