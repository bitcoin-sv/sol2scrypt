{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module IR.Transformations.Sol2IR.Contract where

import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Identifier ()
import IR.Transformations.Sol2IR.Expression ()
import IR.Transformations.Sol2IR.Variable ()
import IR.Transformations.Sol2IR.Function ()
import Solidity.Spec as Sol
import Data.Maybe (catMaybes, fromJust)
import Utils

instance ToIRTransformable ContractDefinition IContract' where
  _toIR (Sol.ContractDefinition "contract" cn [] cps) = do
    cn' <- _toIR cn
    addSym $ Symbol <$> cn' <*> Just contractSymType <*> Just False
    enterScope
    cps' <- mapM _toIR cps
    leaveScope
    return $ Just $ IR.Contract (fromJust cn') (catMaybes  cps')
  _toIR c = error $ "unsupported contract definition `" ++ headWord (show c) ++ "`"


instance ToIRTransformable ContractPart IConstructor' where
  _toIR (Sol.ContractPartConstructorDefinition (ParameterList pl) _ (Just block)) = do
    params' <- mapM _toIR pl
    block' <- _toIR block
    return $ Just $ IR.Constructor (ParamList (catMaybes params')) (fromJust block')
  _toIR c = error $ "unsupported constructor definition `" ++ headWord (show c) ++ "`"

instance ToIRTransformable Sol.PragmaDirective IR.IEmpty where
  _toIR _ = return IR.Empty

instance ToIRTransformable Sol.ContractPart IContractBodyElement' where
  _toIR (Sol.ContractPartStateVariableDeclaration e) = do
    e' :: IStateVariable' <- _toIR e
    addSym $ Symbol <$> (stateVarName <$> e') <*> (stateVarType <$> e') <*> Just True
    return $ Just $ IR.StateVariableDeclaration (fromJust e')
  _toIR Sol.ContractPartEventDefinition {} = return Nothing
  _toIR func@(Sol.ContractPartFunctionDefinition (Just fn) _ _ _ _) = do
    fn' <- _toIR fn
    addSym $ Symbol <$> fn' <*> Just functionSymType <*> Just False
    enterScope
    func' <- _toIR func
    leaveScope
    return $ IR.FunctionDefinition <$> func'
  _toIR ctor@Sol.ContractPartConstructorDefinition {} = do
      ctor' <- _toIR ctor
      return $ IR.ConstructorDefinition <$> ctor'

  _toIR c = error $ "unsupported contract part `" ++ headWord (show c) ++ "`"
