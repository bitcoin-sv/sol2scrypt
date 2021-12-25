{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Variable where

import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Identifier ()
import IR.Transformations.Sol2IR.Expression ()
import IR.Transformations.Sol2IR.Type ()
import Solidity.Spec as Sol
import Data.Maybe (fromJust)

instance ToIRTransformable Parameter IParam' where
  _toIR (Parameter t _ (Just (Sol.Identifier pn))) = do
    t' <- _toIR t
    return $ IR.Param <$> t' <*> Just (IR.Identifier pn)
  _toIR _ = return Nothing



instance ToIRTransformable VariableDeclaration IParam' where
  _toIR (Sol.VariableDeclaration a _ (Sol.Identifier pn)) = do
    t' <- _toIR a
    return $ IR.Param <$> t' <*> Just (IR.Identifier pn)


instance ToIRTransformable Sol.StateVariableDeclaration IStateVariable' where
  _toIR (Sol.StateVariableDeclaration a _ (Sol.Identifier pn) v) = do
    a' :: IType' <- _toIR a
    v' :: IExpr' <- _toIR v
    return $ Just $ IR.StateVariable (IR.Identifier pn) (fromJust a') v'

