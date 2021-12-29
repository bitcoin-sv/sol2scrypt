{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Identifier where

import IR.Spec as IR
import IR.Transformations.Base
import Solidity.Spec as Sol

instance ToIRTransformable Sol.Identifier IIdentifier' where
  _toIR (Sol.Identifier i) = return $ Just $ IR.Identifier i

maybeStateVarId :: IR.IIdentifier' -> Transformation IR.IIdentifier'
maybeStateVarId Nothing = return Nothing
maybeStateVarId (Just i) = do
  s <- lookupSym i
  i' <- case s of
    -- append prefix `this.` for state variables
    Just (Symbol _ _ True) -> return $ IR.Identifier $ "this." ++ IR.unIdentifier i
    _ -> return i
  return $ Just i'