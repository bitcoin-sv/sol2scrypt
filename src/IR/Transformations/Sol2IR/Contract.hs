{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module IR.Transformations.Sol2IR.Contract where

import IR.Spec as IR
import Control.Monad.State
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Identifier ()
import IR.Transformations.Sol2IR.Expression ()
import IR.Transformations.Sol2IR.Variable ()
import IR.Transformations.Sol2IR.Struct ()
import IR.Transformations.Sol2IR.Function (buildPropagateState)
import Solidity.Spec as Sol
import Data.Maybe
import Utils

instance ToIRTransformable (ContractDefinition SourceRange) IContract' where
  _toIR (Sol.ContractDefinition False "contract" cn [] cps _) = do
    cn' <- _toIR cn
    addSym $ Symbol <$> cn' <*> Just contractSymType <*> Just False
    enterScope
    cps' <- mapM _toIR cps
    leaveScope
    let appendPropagateState = findPreimageFunction cps'
    let propagateState = [buildPropagateState | appendPropagateState]
    let cps'' = catMaybes  cps' ++ propagateState
    return $ Just $ IR.Contract (fromJust cn') cps''
  _toIR (Sol.ContractDefinition True _ _ _ _ a) = do
    reportError "unsupported abstract contract definition" a >> return Nothing
  _toIR c = reportError ("unsupported contract definition `" ++ headWord (show c) ++ "`") (ann c) >> return Nothing



instance ToIRTransformable (Sol.PragmaDirective SourceRange) IR.IEmpty where
  _toIR _ = return IR.Empty

instance ToIRTransformable (Sol.ContractPart SourceRange) IContractBodyElement' where
  _toIR (Sol.ContractPartStateVariableDeclaration e _) = do
    e' :: IStateVariable' <- _toIR e
    addSym $ Symbol <$> (stateVarName <$> e') <*> (stateVarType <$> e') <*> Just True
    return $ IR.StateVariableDeclaration <$> e'
  _toIR Sol.ContractPartEventDefinition {} = return Nothing
  _toIR func@(Sol.ContractPartFunctionDefinition (Just fn@(Sol.Identifier i _)) _ _ _ _ a) = do
    fn' <- _toIR fn
    err <- addSym $ Symbol <$> fn' <*> Just functionSymType <*> Just False
    case err of
      Left _ -> reportError ("duplicated function name `" ++ i ++ "` in contract") a >> return Nothing
      Right _ -> do
          enterScope
          func' <- _toIR func
          leaveScope
          return $ IR.FunctionDefinition <$> func'
  _toIR ctor@Sol.ContractPartConstructorDefinition {} = do
      ctor' <- _toIR ctor
      return $ IR.ConstructorDefinition <$> ctor'
  _toIR (Sol.ContractPartStructDefinition st) = do
    _ :: IStruct'  <- _toIR st
    return Nothing
  _toIR c = reportError ("unsupported contract part `" ++ headWord (show c) ++ "`") (ann c) >> return Nothing



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
