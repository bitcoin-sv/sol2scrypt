{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Contract where

import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Identifier ()
import IR.Transformations.Sol2IR.Expression ()
import IR.Transformations.Sol2IR.Variable ()
import IR.Transformations.Sol2IR.Function ()
import Solidity.Spec as Sol
import Data.Maybe (fromJust)

instance ToIRTransformable ContractDefinition IContract' where
  _toIR (Sol.ContractDefinition "contract" cn [] cps) = do
    cn' <- _toIR cn
    cps' <- mapM _toIR cps
    return $ IR.Contract <$> cn' <*> sequence cps'
  _toIR _ = error "unimplemented contract definition"

instance ToIRTransformable Sol.PragmaDirective IR.IEmpty where
  _toIR _ = return IR.Empty

instance ToIRTransformable Sol.ContractPart IContractBodyElement' where
  _toIR (Sol.ContractPartStateVariableDeclaration e) = do
    e' :: IStateVariable' <- _toIR e
    return $ Just $ IR.StateVariableDeclaration (fromJust e')
  _toIR func@Sol.ContractPartFunctionDefinition {} = do
    func' <- _toIR func
    return $ IR.FunctionDefinition <$> func'
  _toIR _ = error "unimplemented contract part"
