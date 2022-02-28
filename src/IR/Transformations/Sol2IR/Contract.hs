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
import IR.Transformations.Sol2IR.Function (buildPropagateState)
import Solidity.Spec as Sol
import Data.Maybe
import Utils
instance ToIRTransformable (ContractDefinition SourceRange) IContract' where
  _toIR (Sol.ContractDefinition "contract" cn [] cps _) = do
    cn' <- _toIR cn
    addSym $ Symbol <$> cn' <*> Just contractSymType <*> Just False
    enterScope
    cps' <- mapM _toIR cps
    leaveScope
    let appendPropagateState = findPreimageFunction cps'
    let propagateState = [buildPropagateState | appendPropagateState]
    let cps'' = catMaybes  cps' ++ propagateState
    return $ Just $ IR.Contract (fromJust cn') cps''
  _toIR c = error $ "unsupported contract definition `" ++ headWord (show c) ++ "`"



instance ToIRTransformable (Sol.PragmaDirective SourceRange) IR.IEmpty where
  _toIR _ = return IR.Empty

instance ToIRTransformable (Sol.ContractPart SourceRange) IContractBodyElement' where
  _toIR (Sol.ContractPartStateVariableDeclaration e _) = do
    e' :: IStateVariable' <- _toIR e
    addSym $ Symbol <$> (stateVarName <$> e') <*> (stateVarType <$> e') <*> Just True
    return $ Just $ IR.StateVariableDeclaration (fromJust e')
  _toIR Sol.ContractPartEventDefinition {} = return Nothing
  _toIR func@(Sol.ContractPartFunctionDefinition (Just fn) _ _ _ _ _) = do
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



findPreimageParam :: IParamList  -> Bool
findPreimageParam (ParamList []) = False
findPreimageParam  (ParamList (x:xs))
  | x == IR.Param (BuiltinType "SigHashPreimage") (IR.ReservedId varTxPreimage) = True
  | otherwise = findPreimageParam $ ParamList xs


findPreimageFunction :: [IContractBodyElement'] -> Bool
findPreimageFunction [] = False
findPreimageFunction (x:xs) = case x of
  Nothing -> findPreimageFunction xs
  Just icbe -> let finded = case icbe of
                          IR.FunctionDefinition (IR.Function _ pl _ _ _) -> findPreimageParam pl
                          _ -> False
                    in  if finded then finded else  findPreimageFunction xs
