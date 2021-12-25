{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IR.Transformations.Sol2IR.VariableDeclaration where

import IR.Transformations.Base
import Solidity.Spec as Sol
import IR.Spec as IR
import IR.Transformations.Sol2IR.Type ()


instance ToIRTransformable VariableDeclaration IParam' where
  _toIR (Sol.VariableDeclaration a _ (Sol.Identifier pn)) = do
    t' <- _toIR a
    return $ IR.Param <$> t' <*> Just (IR.Identifier pn)