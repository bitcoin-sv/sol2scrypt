{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Contract where

import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Expression ()
import IR.Transformations.Sol2IR.Variable ()
import Solidity.Spec as Sol
import Data.Maybe (fromJust)

instance ToIRTransformable Sol.PragmaDirective IR.IEmpty where
  _toIR _ = return IR.Empty


instance ToIRTransformable Sol.ContractPart IContractBodyElement' where
  _toIR (Sol.ContractPartStateVariableDeclaration e) = do
    e' :: IStateVariable' <- _toIR e
    return $ Just $ IR.StateVariableDeclaration (fromJust e')
