{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module IR.Transformations.Sol2IR.Contract where

import Control.Monad.Except
import Control.Monad.State
import Data.Either
import Data.List
import Data.Maybe
import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Expression ()
import IR.Transformations.Sol2IR.Function (buildCheckInitBalanceFunc, buildPropagateStateFunc)
import IR.Transformations.Sol2IR.Identifier ()
import IR.Transformations.Sol2IR.Struct ()
import IR.Transformations.Sol2IR.Variable ()
import Solidity.Parser (display)
import Solidity.Spec as Sol

instance ToIRTransformable (ContractDefinition SourceRange) IContract' where
  _toIR (Sol.ContractDefinition False "contract" cn [] cps _) = do
    -- reset state
    modify $ \s -> s {stateNeedInitBalace = False, stateStatePropertyCount = 0}
    cn' <- _toIR cn
    addSym $ Symbol <$> cn' <*> Just contractSymType <*> Just False <*> Just False <*> Just False
    enterScope
    enterLibrary False
    preprocessFunctionNames cps
    cps' <-
      mapM (_toIR . fst) $
        sortBy
          (\a b -> compare (snd b) (snd a))
          $map
          ( \part -> case part of
              -- functions has lower priority
              ContractPartFunctionDefinition {} -> (part, 0 :: Integer)
              -- ctor has mid priority
              -- bcoz ctor may have `msg.value` which would have effect on other public functions transpiling.
              ContractPartConstructorDefinition {} -> (part, 1)
              -- properties has higher priority
              _ -> (part, 2)
          )
          cps
    leaveScope
    let appendPropagateState = findPreimageFunction cps'
    let propagateStateFunc = [buildPropagateStateFunc | appendPropagateState]

    needInitBalance <- gets stateNeedInitBalace
    let initBalanceParts =
          if needInitBalance
            then
              [ -- add property `initBalance`
                IR.PropertyDefinition $ IR.Property (IR.ReservedId varInitBalance) (ElementaryType IR.Int) Default Nothing (IsConst True) (IsStatic False) (IsState False),
                -- add function `checkInitBalance`
                buildCheckInitBalanceFunc
              ]
            else []

    let cps'' = catMaybes cps' ++ propagateStateFunc ++ initBalanceParts
    return $ Just $ IR.Contract (fromJust cn') cps''
  _toIR (Sol.ContractDefinition True "contract" _ _ _ a) = reportError "unsupported abstract contract definition" a >> return Nothing
  _toIR (Sol.ContractDefinition _ "interface" _ _ _ a) = reportError "unsupported interface definition" a >> return Nothing
  _toIR (Sol.ContractDefinition _ _ _ inherits _ a) | not (null inherits) = reportError "unsupported contract inheritance" a >> return Nothing
  _toIR c = reportError ("unsupported contract definition: `" ++ display c ++ "`") (ann c) >> return Nothing

instance ToIRTransformable (ContractDefinition SourceRange) ILibrary' where
  _toIR (Sol.ContractDefinition False "library" cn [] cps _) = do
    cn' <- _toIR cn
    addSym $ Symbol <$> cn' <*> Just contractSymType <*> Just False <*> Just False <*> Just False
    enterScope
    enterLibrary True
    preprocessFunctionNames cps
    cps' <- mapM _toIR cps
    leaveScope
    return $ Just $ IR.Library (fromJust cn') (catMaybes cps')
  _toIR c = reportError ("unsupported library definition: `" ++ display c ++ "`") (ann c) >> return Nothing

instance ToIRTransformable (Sol.PragmaDirective SourceRange) IR.IEmpty where
  _toIR _ = return IR.Empty

instance ToIRTransformable (Sol.ContractPart SourceRange) IContractBodyElement' where
  _toIR (Sol.ContractPartStateVariableDeclaration e _) = do

    sCount <- gets stateStatePropertyCount
    e' :: IProperty' <- _toIR e

    let isState = fromMaybe False (unState . propIsState <$> e')

    modify $ \s -> s { stateStatePropertyCount = if isState then sCount + 1 else sCount }

    addSym $ Symbol <$> (propName <$> e') <*> (propType <$> e') <*> Just True <*> (unConst . propIsConstant <$> e') <*> (unStatic . propIsStatic <$> e')
    return $ IR.PropertyDefinition <$> e'
  _toIR Sol.ContractPartEventDefinition {} = return Nothing -- TODO: report info: `event definition will be ignored`
  _toIR func@Sol.ContractPartFunctionDefinition {} = do
    enterScope
    func' <- _toIR func
    leaveScope
    return $ IR.FunctionDefinition <$> func'
  _toIR ctor@Sol.ContractPartConstructorDefinition {} = do
    ctor' <- _toIR ctor
    return $ IR.ConstructorDefinition <$> ctor'
  _toIR (Sol.ContractPartStructDefinition st) = do
    _ :: IStruct' <- _toIR st
    return Nothing
  _toIR (Sol.ContractPartUsingForDeclaration _ _ a) = reportError "unsupported using directive" a >> return Nothing
  _toIR (Sol.ContractPartModifierDefinition _ _ _ a) = reportError "unsupported modifier definition" a >> return Nothing
  _toIR (Sol.ContractPartEnumDefinition _ _ a) = reportError "unsupported enum definition" a >> return Nothing

findPreimageParam :: IParamList -> Bool
findPreimageParam (ParamList []) = False
findPreimageParam (ParamList (x : xs))
  | x == IR.Param (BuiltinType "SigHashPreimage") (IR.ReservedId varTxPreimage) = True
  | otherwise = findPreimageParam $ ParamList xs

findPreimageFunction :: [IContractBodyElement'] -> Bool
findPreimageFunction [] = False
findPreimageFunction (x : xs) = case x of
  Nothing -> findPreimageFunction xs
  Just icbe ->
    let finded = case icbe of
          IR.FunctionDefinition (IR.Function _ pl _ _ _ _) -> findPreimageParam pl
          _ -> False
     in if finded then finded else findPreimageFunction xs

-- preprocess function names to support calling function before its definition.
preprocessFunctionNames :: [ContractPart SourceRange] -> Transformation ()
preprocessFunctionNames cps = do
  mapM_
    ( \func -> do
        let (Sol.ContractPartFunctionDefinition (Just fn@(Sol.Identifier i _)) _ _ _ _ a) = func
        fn' <- _toIR fn
        inL <- isInLibrary
        addSymResult <- addSym $ Symbol <$> fn' <*> Just functionSymType <*> Just False <*> Just False <*> Just inL
        when (isLeft addSymResult) $ do
          reportError ("duplicate function name `" ++ i ++ "` in contract") a
    )
    $ filter
      ( \case
          ContractPartFunctionDefinition {} -> True
          _ -> False
      )
      cps