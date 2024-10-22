{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module IR.Transformations.Sol2IR.Function where

import Control.Monad.State
import Data.Foldable
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Set as Set
import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Expression
import IR.Transformations.Sol2IR.Helper
import IR.Transformations.Sol2IR.Identifier ()
import IR.Transformations.Sol2IR.Statement ()
import IR.Transformations.Sol2IR.Type ()
import IR.Transformations.Sol2IR.Variable ()
import Protolude.Monad (concatMapM)
import Solidity.Spec as Sol

data FuncRetTransResult = FuncRetTransResult
  { targetType :: IType',
    originType :: IType',
    name :: IIdentifier'
  }
  deriving (Show, Eq, Ord)

instance ToIRTransformable (ContractPart SourceRange) IFunction' where
  _toIR (ContractPartFunctionDefinition (Just (Sol.Identifier fn a)) pl tags maybeRets (Just block) _) = do
    modify $ \s ->
      s
        { stateInFuncMappingCounter = Map.empty,
          stateInFuncLoopCount = 0,
          stateInFuncBrokeLoops = Set.empty,
          stateInFuncContinuedLoops = Set.empty,
          stateBalanceChanged = False
        }
    vis <- toIRFuncVis tags
    retTransResult <- toIRFuncRet vis maybeRets
    (ps, preimageParams, blkTfromParam) <- toIRFuncParams pl tags retTransResult vis block
    functionBody <- toIRFuncBody fn block vis blkTfromParam retTransResult
    case functionBody of
      Left msg -> reportError msg a >> return Nothing
      Right (ParamList paramsForMap, body) -> do
        let ps' = case ps of
              Just (ParamList pps) -> Just (ParamList $ pps ++ paramsForMap ++ preimageParams)
              _ -> Just $ ParamList $ preimageParams ++ paramsForMap
        inL <- isInLibrary
        return $ Function (IR.Identifier fn) <$> ps' <*> Just body <*> targetType retTransResult <*> Just vis <*> Just inL
  _toIR _ = return Nothing

instance ToIRTransformable (ContractPart SourceRange) IConstructor' where
  _toIR (Sol.ContractPartConstructorDefinition pl tags (Just block) _) = do
    (ps, blkTfromParam) <- toIRConstructorParams pl tags block
    body <- toIRConstructorBody block blkTfromParam
    return $ IR.Constructor <$> ps <*> body
  _toIR c = reportError "unsupported constructor definition without body" (ann c) >> return Nothing

toIRFuncVis :: [FunctionDefinitionTag SourceRange] -> Transformation IVisibility
toIRFuncVis tags
  | Public `elem` tags' = return Public
  | Private `elem` tags' = return Private
  | otherwise = return Default
  where
    tags' =
      map
        ( \tag -> case tag of
            FunctionDefinitionTagStateMutability (StateMutability External _) -> Public
            FunctionDefinitionTagPublic _ -> Public
            FunctionDefinitionTagStateMutability (StateMutability Internal _) -> Default
            FunctionDefinitionTagPrivate _ -> Private
            _ -> Default
        )
        tags

hasMutability :: StateMutability_ -> [FunctionDefinitionTag SourceRange] -> Bool
hasMutability sm tags = sm `elem` tags'
  where
    tags' = map (\(FunctionDefinitionTagStateMutability (StateMutability ability _)) -> ability) tags''
      where
        tags'' =
          filter
            ( \tag -> case tag of
                FunctionDefinitionTagStateMutability _ -> True
                _ -> False
            )
            tags

toIRFuncRet :: IVisibility -> Maybe (ParameterList SourceRange) -> Transformation FuncRetTransResult
toIRFuncRet _ Nothing = return $ FuncRetTransResult (Just $ ElementaryType IR.Bool) Nothing Nothing
toIRFuncRet vis (Just (ParameterList [x])) = do
  n <- case parameterIdentifier x of
    Just i -> _toIR i
    _ -> return Nothing
  t <- _toIR $ parameterType x
  let t' = case vis of
        Public -> Just $ ElementaryType IR.Bool
        _ -> t
  return $ FuncRetTransResult t' t n
toIRFuncRet _ (Just (ParameterList el)) = do
  reportError "unsupported function definition with multi-returns" (mergeRange (ann $ head el) (ann $ last el))
  return $ FuncRetTransResult Nothing Nothing Nothing

toIRFuncParams :: ParameterList SourceRange -> [FunctionDefinitionTag SourceRange] -> FuncRetTransResult -> IVisibility -> Block SourceRange -> Transformation (IParamList', [IParam], TFStmtWrapper)
toIRFuncParams (ParameterList pl) _ (FuncRetTransResult _ ort rn) vis funcBlk = do
  params <- mapM _toIR pl

  -- add parameter for public function whose original return type is not `Bool`
  let extraParams0 = [IR.Param <$> ort <*> rn' | vis == Public && isJust ort]
        where
          rn' = Just $ if isJust rn then mirror rn else IR.Identifier "retVal"

  defaultReturnValue <- defaultValueExpr ort
  let blkT0 = TFStmtWrapper prepends []
        where
          prepends = case rn of
            -- add initialize statement for named return param
            Just _ -> declareLocalVarStmt (IR.Param <$> ort <*> rn) (fromMaybe (LiteralExpr $ BoolLiteral False) defaultReturnValue)
            _ -> []

  -- for function that uses `msg.sender`
  let msgSenderExist = exprExistsInStmt msgSenderExpr (Sol.BlockStatement funcBlk)
  (extraParams1, blkT1) <-
    if fst msgSenderExist
      then do
        if vis == Public
          then let (ps, blkT_) = transForFuncWithMsgSender in return (map Just ps ++ extraParams0, mergeTFStmtWrapper blkT0 blkT_)
          else do
            reportError "unsupported using `msg.sender` in `internal` or `private` function" (snd msgSenderExist)
            return (extraParams0, blkT0)
      else return (extraParams0, blkT0)

  -- for function that uses `msg.value`
  let msgValueExist = exprExistsInStmt msgValueExpr (Sol.BlockStatement funcBlk)
  (extraParams2, blkT2) <-
    if fst msgValueExist
      then
        if vis == Public
          then do
            -- contract's balance got changed if has `msg.value`
            modify $ \s -> s {stateBalanceChanged = True}
            let (ps, blkT_) = transForFuncWithMsgValue
            return (map Just ps ++ extraParams1, mergeTFStmtWrapper blkT1 blkT_)
          else do
            reportError "unsupported using `msg.value` in `internal` or `private` function" (snd msgValueExist)
            return (extraParams1, blkT1)
      else return (extraParams1, blkT1)

  sCount <- gets stateStatePropertyCount
  let needPreimageParam
        | sCount == 0 = False
        | vis == IR.Public = True
        | fst msgValueExist = True
        | otherwise = False
  let (extraParams3, preimageParam, blkT3) =
        if needPreimageParam
          then let (ps, blkT_) = transForPreimageFunc (fst msgValueExist) in (extraParams2, ps, mergeTFStmtWrapper blkT2 blkT_)
          else (extraParams2, [], blkT2)

  let params' = sequence $ params ++ extraParams3

  forM_ params' $ mapM_ (\p -> addSym $ Just $ Symbol (paramName p) (paramType p) False False False)

  return (IR.ParamList <$> params', preimageParam, blkT3)

toIRFuncBody :: String -> Block SourceRange -> IVisibility -> TFStmtWrapper -> FuncRetTransResult -> Transformation (Either String (IParamList, IBlock))
toIRFuncBody fn blk@(Sol.Block _ _) vis wrapperFromParam (FuncRetTransResult _ ort rn) = do
  modify $ \s -> s {stateReturnedInBlock = []}
  blk' <- _toIR blk
  let stmts = case blk' of
        Just (IR.Block ss) -> map Just ss
        Nothing -> []
  returned <- gets stateReturnedInBlock
  let returnedInMiddle = case returned of
        True : _ -> True
        _ -> False

  mCounter <- gets stateInFuncMappingCounter
  transformforMap <- transForMappingAccess fn mCounter vis

  case transformforMap of
    Left msg -> return (Left msg)
    Right (pl, wrapTFSforMap) -> do
      let (TFStmtWrapper prepends appends) = wrapTFStmtWrapper wrapperFromParam wrapTFSforMap
      let hasRetStmt =
            not (null stmts)
              && case last stmts of
                Just (ReturnStmt _) -> True
                _ -> False

      returnExpr <- defaultValueExpr ort

      let stmts' = case (vis == Public, hasRetStmt) of
            (True, True) -> case last stmts of
              Just (ReturnStmt r) -> case ort of
                -- drop `return ;`
                Nothing -> init stmts
                -- use `require(nonBoolExpr == injectedParamName);` to replace `return nonBoolExpr;`
                Just _ ->
                  let e = fromMaybe (IR.Identifier "retVal") rn
                      r' = case r of
                        te@TernaryExpr {} -> ParensExpr te
                        _ -> r
                   in init stmts ++ [Just $ requireEqualStmt r' e]
              _ -> error "last statement is not return statement"
            (True, False) ->
              stmts ++ case rn of
                Just r -> [Just $ requireEqualStmt (IdentifierExpr r) $ mirror rn]
                _ -> []
            (False, False) -> case rn of
              Just n ->
                stmts
                  ++ [ Just $
                         ReturnStmt $
                           if returnedInMiddle
                             then -- append `return returned ? ret : returnName;`

                               TernaryExpr
                                 (IdentifierExpr (IR.ReservedId varReturned))
                                 (IdentifierExpr (IR.ReservedId varRetVal))
                                 (IdentifierExpr n)
                             else -- append `return returnName;`
                               IdentifierExpr n
                     ]
              _ ->
                stmts
                  ++ if returnedInMiddle
                    then -- append `return ret;`
                      [Just $ ReturnStmt $ IdentifierExpr $ IR.ReservedId varRetVal]
                    else -- append `return <defaultValue>;` / `return true;`
                      [Just $ ReturnStmt (fromMaybe (LiteralExpr $ BoolLiteral True) returnExpr)]
            _ -> stmts

      let stmts'' =
            if vis == Public
              then map Just prepends ++ stmts' ++ map Just appends
              else case laststmts of
                ss@(Just (IR.ReturnStmt _)) -> map Just prepends ++ init stmts' ++ map Just appends ++ [ss]
                _ -> map Just prepends ++ stmts' ++ map Just appends
            where
              laststmts = last stmts'

      let stmts''' =
            if vis == Public
              then case reverse stmts'' of
                -- ignore the last `require(true)` if the penultimate is already a `require` stmt
                (Just (RequireStmt (LiteralExpr (BoolLiteral True)))) : (Just (RequireStmt _)) : _ -> init stmts''
                _ -> stmts''
              else stmts''

      preStmts <- prependsForReturnedInit ort
      let stmts4 =
            if returnedInMiddle
              then map Just preStmts ++ stmts'''
              else stmts'''

      balanceChanged <- gets stateBalanceChanged
      let stmts5 =
            if balanceChanged
              then -- prepends `contractBalance` declaration
                declareBalance : stmts4
              else stmts4
            where
              -- `int contractBalance = Sighash.value(txPreimage) + msgValue;`
              declareBalance =
                Just $
                  IR.DeclareStmt
                    [IR.Param (ElementaryType IR.Int) (IR.ReservedId varContractBalance)]
                    [ IR.BinaryExpr
                        Add
                        (libCallExpr libSigHash varValue [ridExpr varTxPreimage])
                        (ridExpr varMsgValue)
                    ]

      needCheckInitBalance <- gets stateNeedInitBalace
      let stmts6 =
            -- check initial balance in public functions if needed
            if needCheckInitBalance && vis == Public
              then -- prepends checkInitBalance
                callCheckInitBalance : stmts5
              else stmts5
            where
              -- call `require(this.checkInitBalance(txPreimage));`
              callCheckInitBalance =
                Just $
                  IR.RequireStmt $
                    IR.FunctionCallExpr
                      (IR.MemberAccessExpr thisExpr (IR.ReservedId funcCheckInitBalance))
                      [ridExpr varTxPreimage]

      let stmts7 = if vis == Public 
                    then 
                      if null stmts6 then
                        [Just (IR.RequireStmt (IR.LiteralExpr (IR.BoolLiteral True)))]
                      else 
                        case last stmts6 of
                          (Just (IR.RequireStmt _)) -> stmts6
                          _ -> stmts6 ++ [Just (IR.RequireStmt (IR.LiteralExpr (IR.BoolLiteral True)))]
                    else 
                      stmts6

      return $ Right (ParamList pl, IR.Block $ catMaybes stmts7)

mirror :: IIdentifier' -> IIdentifier
mirror (Just (IR.Identifier i)) = IR.Identifier ("_" ++ i)
mirror _ = error "try to mirror an identifier from nothing"

requireEqualStmt :: IExpression -> IIdentifier -> IStatement
requireEqualStmt e i = RequireStmt $ BinaryExpr IR.Equal e $ IdentifierExpr i

trueExpr :: IExpression
trueExpr = LiteralExpr $ BoolLiteral True

declareLocalVarStmt :: IParam' -> IExpression -> [IStatement]
declareLocalVarStmt Nothing _ = []
declareLocalVarStmt (Just p) e = [IR.DeclareStmt [p] [e]]

varValue :: String
varValue = "value"

-- transformations for functions that need access preimage
transForPreimageFunc :: Bool -> ([IParam], TFStmtWrapper)
transForPreimageFunc balanceChanged =
  ( [IR.Param (BuiltinType "SigHashPreimage") $ IR.ReservedId varTxPreimage],
    TFStmtWrapper
      []
      -- appends statements
      [ -- require(this.propagateState(preimage, <balanceChanged ? SigHash.value(preimage) + msgValue: SigHash.value(preiamge)> ));
        IR.RequireStmt $
          IR.FunctionCallExpr
            (IR.MemberAccessExpr thisExpr (IR.ReservedId funcPropagateState))
            [ IdentifierExpr (IR.ReservedId varTxPreimage),
              let prevBalance =
                    IR.FunctionCallExpr
                      (IR.MemberAccessExpr (IdentifierExpr (IR.ReservedId libSigHash)) (IR.Identifier varValue))
                      [IdentifierExpr (IR.ReservedId varTxPreimage)]
               in if balanceChanged
                    then IdentifierExpr $ IR.ReservedId varContractBalance
                    else prevBalance
            ]
      ]
  )

-- transformations for functions that uses `msg.sender`
transForFuncWithMsgSender :: ([IParam], TFStmtWrapper)
transForFuncWithMsgSender =
  ( [ IR.Param (BuiltinType "Sig") $ IR.ReservedId varSig,
      IR.Param (BuiltinType "PubKey") $ IR.ReservedId varPubKey
    ],
    TFStmtWrapper
      [ -- PubKeyHash msgSender = hash160(pubKey);
        IR.DeclareStmt
          [IR.Param (ElementaryType IR.Address) (IR.ReservedId varMsgSender)]
          [IR.FunctionCallExpr (IdentifierExpr (IR.ReservedId funcHash160)) [IdentifierExpr (IR.ReservedId varPubKey)]],
        -- require(checkSig(sig, pubKey));
        IR.RequireStmt $
          IR.FunctionCallExpr (IdentifierExpr (IR.ReservedId funcCheckSig)) [IdentifierExpr (IR.ReservedId varSig), IdentifierExpr (IR.ReservedId varPubKey)]
      ] -- end of prepends
      [] -- appends
  )

-- transformations for functions that uses `msg.value`
transForFuncWithMsgValue :: ([IParam], TFStmtWrapper)
transForFuncWithMsgValue =
  ( [IR.Param (ElementaryType IR.Int) $ IR.ReservedId varMsgValue],
    TFStmtWrapper
      [ -- require(msgValue >= 0);
        IR.RequireStmt $ IR.BinaryExpr IR.GreaterThanOrEqual (ridExpr varMsgValue) $ LiteralExpr $ IR.IntLiteral False 0
      ]
      []
  )

msgSenderExpr :: Sol.Expression SourceRange
msgSenderExpr = Sol.MemberAccess (Sol.Literal (PrimaryExpressionIdentifier (Sol.Identifier "msg" defaultSourceRange))) (Sol.Identifier "sender" defaultSourceRange) defaultSourceRange

msgValueExpr :: Sol.Expression SourceRange
msgValueExpr = Sol.MemberAccess (Sol.Literal (PrimaryExpressionIdentifier (Sol.Identifier "msg" defaultSourceRange))) (Sol.Identifier varValue defaultSourceRange) defaultSourceRange

toIRConstructorParams :: ParameterList SourceRange -> [FunctionDefinitionTag SourceRange] -> Block SourceRange -> Transformation (IParamList', TFStmtWrapper)
toIRConstructorParams (ParameterList pl) _ funcBlk = do
  params <- mapM _toIR pl

  let extraParams0 = []

  let blkT0 = TFStmtWrapper [] []

  -- for function that uses `msg.sender`
  let (extraParams1, blkT1) =
        if fst (exprExistsInStmt msgSenderExpr (Sol.BlockStatement funcBlk))
          then let (ps, blkT_) = transForConstructorWithMsgSender in (map Just ps ++ extraParams0, mergeTFStmtWrapper blkT0 blkT_)
          else (extraParams0, blkT0)

  -- for function that uses `msg.value`
  let msgValueExist = exprExistsInStmt msgValueExpr (Sol.BlockStatement funcBlk)
  (extraParams2, blkT2) <-
    if fst msgValueExist
      then do
        -- set `stateNeedInitBalace` to true,
        -- thus the initial balance assigned by `msg.value` can be checked in other public functions.
        modify $ \s -> s {stateNeedInitBalace = True}
        let (ps, blkT_) = transForConstructorWithMsgValue
        return (map Just ps ++ extraParams1, mergeTFStmtWrapper blkT1 blkT_)
      else return (extraParams1, blkT1)

  let params' = sequence $ params ++ extraParams2

  return (IR.ParamList <$> params', blkT2)

toIRConstructorBody :: Block SourceRange -> TFStmtWrapper -> Transformation IBlock'
toIRConstructorBody (Sol.Block ss _) (TFStmtWrapper prepends appends) = do
  stmts' <- concatMapM _toIR ss
  let stmts'' = map Just prepends ++ stmts' ++ map Just appends
  return $ Just $ IR.Block $ catMaybes stmts''

-- transformations for constructor that uses `msg.sender`
transForConstructorWithMsgSender :: ([IR.IParam], TFStmtWrapper)
transForConstructorWithMsgSender =
  ( [IR.Param (ElementaryType Address) $ IR.ReservedId varMsgSender],
    TFStmtWrapper
      [] -- prepends
      [] -- appends
  )

-- transformations for constructor that uses `msg.value`
transForConstructorWithMsgValue :: ([IR.IParam], TFStmtWrapper)
transForConstructorWithMsgValue =
  ( [IR.Param (ElementaryType IR.Int) $ IR.ReservedId varMsgValue],
    TFStmtWrapper
      []
      -- append `this.initBalance = msgValue;` to set the initial contract balance
      [ IR.AssignStmt
          [IR.MemberAccessExpr thisExpr (IR.ReservedId varInitBalance)]
          [IR.IdentifierExpr $ IR.ReservedId varMsgValue]
      ]
  )

-- <mapExpr>.set({<keyExpr>, <idxExpr>}, <valExpr>)
mapSetExpr :: IExpression -> IExpression -> String -> Int -> IExpression
mapSetExpr mapExpr keyExpr postfix idx =
  let e = Just $ BinaryExpr Index mapExpr keyExpr
   in FunctionCallExpr
        { funcExpr =
            MemberAccessExpr
              { instanceExpr = mapExpr,
                member = IR.Identifier "set"
              },
          funcParamExprs =
            [ StructLiteralExpr [keyExpr, fromJust $ indexExprOfMapping idx],
              fromJust $ valueExprOfMapping e postfix
            ]
        }

-- -- require(<mapExpr>.set({keyExpr, idxExpr}, valExpr))
afterCheckStmt :: IExpression -> IExpression -> String -> Int -> IStatement
afterCheckStmt mapExpr keyExpr postfix i =
  IR.RequireStmt $ mapSetExpr mapExpr keyExpr postfix i

transForMappingAccess :: String -> MappingExprCounter -> IVisibility -> Transformation (Either String ([IR.IParam], TFStmtWrapper))
transForMappingAccess fn mCounter vis = do
  let initTag = ""
  -- injected params
  let injectedParams =
        concatMap
          ( \(MECEntry t me ke _ _ i) ->
              let e = Just $ BinaryExpr Index me ke
               in [ IR.Param t $ IR.Identifier $ fromJust $ valueNameOfMapping e initTag, -- init value
                    IR.Param (ElementaryType Int) $ IR.Identifier ("i" ++ show i)
                  ]
          )
          $ Map.elems mCounter

  -- injected statements
  injectedStatements <-
    foldlM
      ( \mc (MECEntry _ me ke _ updated i) -> do
          return $ mergeTFStmtWrapper' mc (Just (TFStmtWrapper [] [afterCheckStmt me ke initTag i | updated]))
      )
      (Just (TFStmtWrapper [] []))
      mCounter

  case injectedStatements of
    Just ss ->
      if (vis == Private || vis == Default) && not (null injectedParams)
        then return $ Left ("accessing mapping expression in `internal` or `private` function `" ++ fn ++ "` is not supported")
        else return $ Right (injectedParams, ss)
    Nothing -> return $ Left "injected statements failed when transpiling mapping"

-- prepends some init statements for functions that have returned in middle.
prependsForReturnedInit :: IType' -> Transformation [IStatement]
prependsForReturnedInit t = do
  e <- defaultValueExpr t
  let t' = case t of
        Nothing -> ElementaryType Bool
        Just a -> a
  return
    [ -- `<T> ret = <defaultValueExprueofT>;`
      IR.DeclareStmt
        [IR.Param t' (IR.ReservedId varRetVal)]
        [fromMaybe (LiteralExpr $ BoolLiteral False) e],
      -- `bool returned = false;`
      IR.DeclareStmt
        [IR.Param (ElementaryType IR.Bool) (IR.ReservedId varReturned)]
        [LiteralExpr $ BoolLiteral False]
    ]

-- build `propagateState` function, in this way we can call `require(this.propagateState(txPreimage));` in other public functions
buildPropagateStateFunc :: IR.IContractBodyElement
buildPropagateStateFunc =
  IR.FunctionDefinition $
    IR.Function
      (IR.ReservedId funcPropagateState)
      ( IR.ParamList
          [ IR.Param (BuiltinType "SigHashPreimage") $ IR.ReservedId varTxPreimage,
            IR.Param (ElementaryType IR.Int) $ IR.Identifier varValue
          ]
      )
      (IR.Block body)
      (ElementaryType Bool)
      Default
      False
  where
    body =
      [ -- add `require(Tx.checkPreimage(txPreimage));`
        IR.RequireStmt $
          IR.FunctionCallExpr
            (IR.MemberAccessExpr (IdentifierExpr (IR.ReservedId libTx)) (IR.Identifier "checkPreimage"))
            [IdentifierExpr (IR.ReservedId varTxPreimage)],
        -- add `bytes outputScript = this.getStateScript();`
        IR.DeclareStmt
          [IR.Param (ElementaryType IR.Bytes) (IR.ReservedId varOutputScript)]
          [ IR.FunctionCallExpr
              (IR.MemberAccessExpr thisExpr (IR.Identifier "getStateScript"))
              []
          ],
        -- add `bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));`
        IR.DeclareStmt
          [IR.Param (ElementaryType IR.Bytes) (IR.ReservedId varOutput)]
          [ IR.FunctionCallExpr
              (IR.MemberAccessExpr (IdentifierExpr (IR.ReservedId libUtils)) (IR.Identifier "buildOutput"))
              [ IdentifierExpr (IR.ReservedId varOutputScript),
                IdentifierExpr (IR.Identifier varValue)
              ]
          ],
        -- add `retrun hash256(output) == SigHash.hashOutputs(txPreimage);`
        IR.ReturnStmt $
          IR.BinaryExpr
            IR.Equal
            (IR.FunctionCallExpr (IdentifierExpr (IR.ReservedId funcHash256)) [IdentifierExpr (IR.ReservedId varOutput)])
            $ IR.FunctionCallExpr
              (IR.MemberAccessExpr (IdentifierExpr (IR.ReservedId libSigHash)) (IR.Identifier "hashOutputs"))
              [IdentifierExpr (IR.ReservedId varTxPreimage)]
      ]

{--
  Build function as below:
    function checkInitBalance(SigHashPreimage txPreimage) : bool {
      return !VarIntReader.isFirstCall(txPreimage) || SigHash.value(txPreimage) == this.initBalance;
    }

  in this way, we can call `require(this.checkInitBalance(txPreimage));` in other public functions
--}
buildCheckInitBalanceFunc :: IR.IContractBodyElement
buildCheckInitBalanceFunc =
  IR.FunctionDefinition $
    IR.Function
      (IR.ReservedId funcCheckInitBalance)
      ( IR.ParamList
          [IR.Param (BuiltinType "SigHashPreimage") $ IR.ReservedId varTxPreimage]
      )
      (IR.Block body)
      (ElementaryType Bool)
      Default
      False
  where
    body =
      [ IR.ReturnStmt $
          IR.BinaryExpr
            IR.BoolOr
            (IR.UnaryExpr IR.Not (libCallExpr libTx "isFirstCall" [ridExpr varTxPreimage]))
            ( IR.BinaryExpr
                IR.Equal
                (libCallExpr libSigHash varValue [ridExpr varTxPreimage])
                (IR.MemberAccessExpr thisExpr (IR.ReservedId varInitBalance))
            )
      ]

-- id expression
idExpr :: String -> IExpression
idExpr = IR.IdentifierExpr . IR.Identifier

-- reserved id expression
ridExpr :: String -> IExpression
ridExpr = IR.IdentifierExpr . IR.ReservedId

-- library function call expression
libCallExpr :: String -> String -> [IExpression] -> IExpression
libCallExpr lib func = IR.FunctionCallExpr (IR.MemberAccessExpr (ridExpr lib) $ IR.Identifier func)

-- `this` expression
thisExpr :: IExpression
thisExpr = idExpr "this"