{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IR.Transformations.Sol2IR.Function where

import Control.Monad.State
import qualified Data.Map.Lazy as Map
import Data.Maybe
import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Expression
import IR.Transformations.Sol2IR.Helper
import IR.Transformations.Sol2IR.Identifier ()
import IR.Transformations.Sol2IR.Statement ()
import IR.Transformations.Sol2IR.Type ()
import IR.Transformations.Sol2IR.Variable ()
import Solidity.Spec as Sol
import Protolude.Monad (concatMapM)
import Data.Foldable
import qualified Data.Set as Set

data FuncRetTransResult = FuncRetTransResult
  { targetType :: IType',
    originType :: IType',
    name :: IIdentifier'
  }
  deriving (Show, Eq, Ord)

instance ToIRTransformable (ContractPart SourceRange) IFunction' where
  _toIR (ContractPartFunctionDefinition (Just (Sol.Identifier fn a)) pl tags maybeRets (Just block) _) = do
    modify $ \s -> s {stateInFuncMappingCounter = Map.empty, stateInFuncLoopCount = 0, stateInFuncBrokeLoops = Set.empty, stateInFuncContinuedLoops = Set.empty}
    vis <- toIRFuncVis tags
    retTransResult <- toIRFuncRet vis maybeRets
    (ps, blkTfromParam) <- toIRFuncParams pl tags retTransResult vis block
    functionBody <- toIRFuncBody block vis blkTfromParam retTransResult
    case functionBody of
      Left msg -> reportError msg a >> return Nothing
      Right (ParamList paramsForMap, body) -> do
        let ps' = case ps of
                Just (ParamList pps) -> Just (ParamList $ pps ++ paramsForMap)
                _ -> Just $ ParamList paramsForMap
        inL <- isInLibrary
        return $ Function (IR.Identifier fn) <$> ps' <*> Just body <*> targetType retTransResult <*> Just vis <*> Just inL

  _toIR _ = return Nothing

instance ToIRTransformable (ContractPart SourceRange) IConstructor' where
  _toIR (Sol.ContractPartConstructorDefinition pl tags (Just block) _) = do
    (ps, blkTfromParam) <- toIRConstructorParams pl tags block
    body <- toIRConstructorBody block blkTfromParam
    return $ IR.Constructor <$> ps <*> body
  _toIR c = reportError "unsupported constructor definition: missing body" (ann c) >> return Nothing

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
            FunctionDefinitionTagStateMutability (StateMutability Internal _) -> Private
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
  reportError "unsupported function definition: mutiple-returns" (mergeRange (ann $ head el) (ann $ last el))
  return $ FuncRetTransResult Nothing Nothing Nothing

toIRFuncParams :: ParameterList SourceRange -> [FunctionDefinitionTag SourceRange] -> FuncRetTransResult -> IVisibility -> Block SourceRange -> Transformation (IParamList', TFStmtWrapper)
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
              then
                let (ps, blkT_) = transForFuncWithMsgSender in return (map Just ps ++ extraParams0, mergeTFStmtWrapper blkT0 blkT_)
              else do
                reportError "using `msg.sender` in non-external function is not supported yet" (snd msgSenderExist)
                return (extraParams0, blkT0)
          else return (extraParams0, blkT0)

  -- for function that uses `msg.value`
  let msgValueExist = exprExistsInStmt msgValueExpr (Sol.BlockStatement funcBlk)
  (extraParams2, blkT2) <-
        if fst msgValueExist
          then
            if vis == Public
              then let (ps, blkT_) = transForFuncWithMsgValue in return (map Just ps ++ extraParams1, mergeTFStmtWrapper blkT1 blkT_)
              else do
                reportError "using `msg.value` in non-external function is not supported yet" (snd msgValueExist)
                return (extraParams1, blkT1)
          else return (extraParams1, blkT1)

  let needPreimageParam
        | vis == IR.Public = True
        | fst msgValueExist = True
        | otherwise = False
  let (extraParams3, blkT3) =
        if needPreimageParam
          then let (ps, blkT_) = transForPreimageFunc in (map Just ps ++ extraParams2, mergeTFStmtWrapper blkT2 blkT_)
          else (extraParams2, blkT2)

  let params' = sequence $ params ++ extraParams3

  forM_ params' $ mapM_ (\p -> addSym $ Just $ Symbol (paramName p) (paramType p) False False)

  return (IR.ParamList <$> params', blkT3)

toIRFuncBody :: Block SourceRange -> IVisibility -> TFStmtWrapper -> FuncRetTransResult -> Transformation (Either String (IParamList, IBlock))
toIRFuncBody blk@(Sol.Block _ _) vis wrapperFromParam (FuncRetTransResult _ ort rn) = do
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
  transformforMap <- transForMappingAccess mCounter vis

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
                Nothing  -> init stmts
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

      let stmts'' = if vis == Public then map Just prepends ++ stmts' ++ map Just appends
                    else case laststmts of
                          ss@(Just (IR.ReturnStmt _)) -> map Just prepends ++ init stmts' ++ map Just appends ++ [ss]
                          _ -> map Just prepends ++ stmts' ++ map Just appends
                          where
                            laststmts = last stmts'

      let stmts''' = if vis == Public then
                case reverse stmts'' of
                  -- ignore the last `require(true)` if the penultimate is already a `require` stmt
                  (Just (RequireStmt (LiteralExpr (BoolLiteral True)))) : (Just (RequireStmt _)) : _ -> init stmts''
                  _ -> stmts''
                else stmts''


      preStmts <- prependsForReturnedInit ort
      let stmts4 =
            if returnedInMiddle
              then map Just preStmts ++ stmts'''
              else stmts'''

      return $ Right (ParamList pl, IR.Block $ catMaybes stmts4)

mirror :: IIdentifier' -> IIdentifier
mirror (Just (IR.Identifier i)) = IR.Identifier ("_" ++ i)
mirror _ = error "try to mirror an identifier from nothing"

requireEqualStmt :: IExpression -> IIdentifier -> IStatement
requireEqualStmt e i = RequireStmt $ BinaryExpr IR.Equal e $ IdentifierExpr i

trueExpr :: IExpression
trueExpr = LiteralExpr $ BoolLiteral True

declareLocalVarStmt :: IParam' -> IExpression -> [IStatement]
declareLocalVarStmt Nothing _ = []
declareLocalVarStmt (Just p) e = [IR.DeclareStmt [Just p] [e]]

-- transformations for functions that need access preimage
transForPreimageFunc :: ([IParam], TFStmtWrapper)
transForPreimageFunc =
  ( [IR.Param (BuiltinType "SigHashPreimage") $ IR.ReservedId varTxPreimage],
    TFStmtWrapper
      []
      -- appends statements
      [ -- require(this.propagateState(preimage));
        IR.RequireStmt $
          IR.FunctionCallExpr
            (IR.MemberAccessExpr (IdentifierExpr (IR.Identifier "this")) (IR.Identifier "propagateState"))
            [IdentifierExpr (IR.ReservedId varTxPreimage)]
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
          [Just $ IR.Param (ElementaryType IR.Address) (IR.ReservedId varMsgSender)]
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
  ( [],
    -- int msgValue = SigHash.value(txPreimage);
    TFStmtWrapper
      [ IR.DeclareStmt
          [Just $ IR.Param (ElementaryType IR.Int) (IR.ReservedId varMsgValue)]
          [ IR.FunctionCallExpr
              (IR.MemberAccessExpr (IdentifierExpr (IR.ReservedId libSigHash)) (IR.Identifier "value"))
              [IdentifierExpr (IR.ReservedId varTxPreimage)]
          ]
      ]
      []
  )

msgSenderExpr :: Sol.Expression SourceRange
msgSenderExpr = Sol.MemberAccess (Sol.Literal (PrimaryExpressionIdentifier (Sol.Identifier "msg" defaultSourceRange))) (Sol.Identifier "sender" defaultSourceRange) defaultSourceRange

msgValueExpr :: Sol.Expression SourceRange
msgValueExpr = Sol.MemberAccess (Sol.Literal (PrimaryExpressionIdentifier (Sol.Identifier "msg" defaultSourceRange))) (Sol.Identifier "value" defaultSourceRange) defaultSourceRange

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

  let params' = sequence $ params ++ extraParams1

  return (IR.ParamList <$> params', blkT1)

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
  ( [IR.Param (ElementaryType Int) $ IR.ReservedId varMsgValue],
    TFStmtWrapper
      [] -- prepends
      [] -- appends
  )


 -- -- <mapExpr>.canGet(<keyExpr>, <valExpr>, <idxExpr>)
mapCanGetExpr :: IExpression -> IExpression -> String -> IExpression
mapCanGetExpr mapExpr keyExpr postfix =
  let e = Just $ BinaryExpr Index mapExpr keyExpr
  in FunctionCallExpr
        { funcExpr =
            MemberAccessExpr
              { instanceExpr = mapExpr,
                member = IR.Identifier "canGet"
              },
          funcParamExprs =
            [ keyExpr,
              fromJust $ valueExprOfMapping e postfix,
              fromJust $ indexExprOfMapping e postfix
            ]
        }

-- <mapExpr>.set(<keyExpr>, <valExpr>, <idxExpr>)
mapSetExpr :: IExpression -> IExpression -> String -> IExpression
mapSetExpr mapExpr keyExpr postfix =
  let e = Just $ BinaryExpr Index mapExpr keyExpr
  in FunctionCallExpr
        { funcExpr =
            MemberAccessExpr
              { instanceExpr = mapExpr,
                member = IR.Identifier "set"
              },
          funcParamExprs =
            [ keyExpr,
              fromJust $ valueExprOfMapping e postfix,
              fromJust $ indexExprOfMapping e postfix
            ]
        }

-- -- require((!<mapExpr>.has(<keyExpr>, <idxExpr>)) || <mapExpr>.canGet(<keyExpr>, <valExpr>, <idxExpr>));
preCheckStmt :: IType' -> IExpression -> IExpression -> String -> Transformation IStatement
preCheckStmt t mapExpr keyExpr postfix = do
  defaultValue <- defaultValueExpr t
  return $ IR.RequireStmt $
          BinaryExpr
            { binaryOp = BoolOr,
              lExpr =
                ParensExpr
                  { enclosedExpr =
                      BinaryExpr {

                        binaryOp = BoolAnd ,
                        lExpr = UnaryExpr
                        { unaryOp = Not,
                          uExpr =
                            FunctionCallExpr
                              { funcExpr =
                                  MemberAccessExpr
                                    { instanceExpr = mapExpr,
                                      member = IR.Identifier "has"
                                    },
                                funcParamExprs =
                                  [ keyExpr,
                                    fromJust $ indexExprOfMapping e postfix
                                  ]
                              }
                        },
                        rExpr = BinaryExpr {
                          binaryOp = IR.Equal,
                          lExpr = fromJust $ valueExprOfMapping e postfix,
                          rExpr = fromMaybe (LiteralExpr $ BoolLiteral False) defaultValue
                        }
                      }

                  },
              rExpr = mapCanGetExpr mapExpr keyExpr postfix
            }
            where
                e = Just $ BinaryExpr Index mapExpr keyExpr

-- -- require(<mapExpr>.set(keyExpr, valExpr, idxExpr))
afterCheckStmt :: IExpression -> IExpression -> String -> IStatement
afterCheckStmt mapExpr keyExpr postfix =
  IR.RequireStmt $ mapSetExpr mapExpr keyExpr postfix


transForMappingAccess :: MappingExprCounter -> IVisibility -> Transformation (Either String ([IR.IParam], TFStmtWrapper))
transForMappingAccess mCounter vis = do
  let initTag = ""
  -- injected params
  let injectedParams = concatMap ( \(MECEntry t me ke _ _) ->
          let e = Just $ BinaryExpr Index me ke
           in [ IR.Param t $ IR.Identifier $ fromJust $ valueNameOfMapping e initTag, -- init value
                IR.Param (ElementaryType Int) $ IR.Identifier $ fromJust $ indexNameOfMapping e initTag -- init value index
              ]
        ) $ Map.elems mCounter

  -- injected statements
  injectedStatements <- foldlM ( \mc (MECEntry t me ke _ updated) -> do
          preCheckStmt' <- preCheckStmt (Just t) me ke initTag
          return $ mergeTFStmtWrapper' mc (Just (TFStmtWrapper [preCheckStmt'] [afterCheckStmt me ke initTag | updated]))
        )
        (Just (TFStmtWrapper [] []))
        mCounter

  case injectedStatements of
    Just ss -> if (vis == Private || vis == Default) && not (null injectedParams)
      then
        return $ Left "accessing mapping expression in non-external function is not supported"
      else
        return $ Right (injectedParams,  ss)
    Nothing -> return $ Left "injected statements failed when transpiling mapping"




-- prepends some init statements for functions that have returned in middle.
prependsForReturnedInit :: IType' -> Transformation [IStatement]
prependsForReturnedInit t = do
  e <- defaultValueExpr t
  let t' = case t of
          Nothing -> ElementaryType Bool
          Just a -> a
  return [ -- `<T> ret = <defaultValueExprueofT>;`
    IR.DeclareStmt
      [Just $ IR.Param t' (IR.ReservedId varRetVal)]
      [fromMaybe (LiteralExpr $ BoolLiteral False) e],
    -- `bool returned = false;`
    IR.DeclareStmt
      [Just $ IR.Param (ElementaryType IR.Bool) (IR.ReservedId varReturned)]
      [LiteralExpr $ BoolLiteral False]
    ]


-- build `propagateState` function, in this way we can call `require(this.propagateState(txPreimage));` in other public functions
buildPropagateState :: IR.IContractBodyElement
buildPropagateState =
  IR.FunctionDefinition $
    IR.Function
      (IR.Identifier "propagateState")
      (IR.ParamList [IR.Param (BuiltinType "SigHashPreimage") $ IR.ReservedId varTxPreimage])
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
          [Just $ IR.Param (ElementaryType IR.Bytes) (IR.ReservedId varOutputScript)]
          [ IR.FunctionCallExpr
              (IR.MemberAccessExpr (IdentifierExpr (IR.Identifier "this")) (IR.Identifier "getStateScript"))
              []
          ],
        -- add `bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));`
        IR.DeclareStmt
          [Just $ IR.Param (ElementaryType IR.Bytes) (IR.ReservedId varOutput)]
          [ IR.FunctionCallExpr
              (IR.MemberAccessExpr (IdentifierExpr (IR.ReservedId libUtils)) (IR.Identifier "buildOutput"))
              [ IdentifierExpr (IR.ReservedId varOutputScript),
                IR.FunctionCallExpr
                  (IR.MemberAccessExpr (IdentifierExpr (IR.ReservedId libSigHash)) (IR.Identifier "value"))
                  [IdentifierExpr (IR.ReservedId varTxPreimage)]
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
