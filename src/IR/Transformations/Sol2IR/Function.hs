{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Function where

import Data.Foldable
import Data.Maybe
import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Helper
import IR.Transformations.Sol2IR.Identifier ()
import IR.Transformations.Sol2IR.Statement ()
import IR.Transformations.Sol2IR.Type ()
import IR.Transformations.Sol2IR.Variable ()
import Solidity.Spec as Sol
import Utils
import Debug.Trace

data TransformationOnBlock = TransformationOnBlock
  { prependStmts :: [IR.IStatement],
    appendStmts :: [IR.IStatement]
  }
  deriving (Show, Eq, Ord)

data FuncRetTransResult = FuncRetTransResult
  { targetType :: IType',
    originType :: IType',
    name :: IIdentifier'
  }
  deriving (Show, Eq, Ord)

instance ToIRTransformable ContractPart IFunction' where
  _toIR (ContractPartFunctionDefinition (Just (Sol.Identifier fn)) pl tags maybeRets (Just block)) = do
    clearReturned
    vis <- toIRFuncVis tags
    retTransResult <- toIRFuncRet vis maybeRets
    (ps, blkTfromParam) <- toIRFuncParams pl tags retTransResult vis block
    body <- toIRFuncBody block vis blkTfromParam retTransResult
    return $ Function (IR.Identifier fn) <$> ps <*> body <*> targetType retTransResult <*> Just vis
  _toIR _ = return Nothing


instance ToIRTransformable ContractPart IConstructor' where
  _toIR (Sol.ContractPartConstructorDefinition pl tags (Just block)) = do
    (ps, blkTfromParam) <- toIRConstructorParams pl tags block
    body <- toIRConstructorBody block blkTfromParam
    return $ IR.Constructor <$> ps <*> body
  _toIR c = error $ "unsupported constructor definition `" ++ headWord (show c) ++ "`"

toIRFuncVis :: [FunctionDefinitionTag] -> Transformation IVisibility
toIRFuncVis tags
  | FunctionDefinitionTagStateMutability External `elem` tags = return Public
  | FunctionDefinitionTagPrivate `elem` tags || FunctionDefinitionTagStateMutability Internal `elem` tags = return Private
  | otherwise = return Default

toIRFuncRet :: IVisibility -> Maybe ParameterList -> Transformation FuncRetTransResult
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
toIRFuncRet _ _ = return $ FuncRetTransResult Nothing Nothing Nothing -- error "mutiple-returns function not implemented"

toIRFuncParams :: ParameterList -> [FunctionDefinitionTag] -> FuncRetTransResult -> IVisibility -> Block -> Transformation (IParamList', TransformationOnBlock)
toIRFuncParams (ParameterList pl) tags (FuncRetTransResult rt ort rn) vis funcBlk = do
  params <- mapM _toIR pl

  -- add parameter for public function whose original return type is not `Bool`
  let extraParams0 = [IR.Param <$> ort <*> rn' | vis == Public && isJust ort && ort /= rt]
        where
          rn' = Just $ if isJust rn then mirror rn else IR.Identifier "retVal"

  let blkT0 = TransformationOnBlock prepends []
        where
          prepends = case rn of
            -- add initialize statement for named return param
            Just _ -> declareLocalVarStmt $ IR.Param <$> ort <*> rn
            _ -> []

  -- for function that uses `msg.sender`
  let (extraParams1, blkT1) =
        if exprExistsInStmt msgSenderExpr (Sol.BlockStatement funcBlk)
          then
            if vis == Public
              then let (ps, blkT_) = transForFuncWithMsgSender in (map Just ps ++ extraParams0, mergeTransOnBlock blkT0 blkT_)
              else error "using `msg.sender` in non-external function is not supported yet"
          else (extraParams0, blkT0)

  -- for function that uses `msg.value`
  let msgValueExist = exprExistsInStmt msgValueExpr (Sol.BlockStatement funcBlk)
  let (extraParams2, blkT2) =
        if msgValueExist
          then
            if vis == Public
              then let (ps, blkT_) = transForFuncWithMsgValue in (map Just ps ++ extraParams1, mergeTransOnBlock blkT1 blkT_)
              else error "using `msg.value` in non-external function is not supported yet"
          else (extraParams1, blkT1)

  let needPreimageParam
        | FunctionDefinitionTagStateMutability Pure `elem` tags = False -- pure function do ont need preimage
        | vis == IR.Public = True
        | msgValueExist = True
        | otherwise = False
  let (extraParams3, blkT3) =
        if needPreimageParam
          then let (ps, blkT_) = transForPreimageFunc in (map Just ps ++ extraParams2, mergeTransOnBlock blkT2 blkT_)
          else (extraParams2, blkT2)

  let params' = sequence $ params ++ extraParams3

  forM_ params' $ mapM_ (\p -> addSym $ Just $ Symbol (paramName p) (paramType p) False)

  return (IR.ParamList <$> params', blkT3)

toIRFuncBody :: Block -> IVisibility -> TransformationOnBlock -> FuncRetTransResult -> Transformation IBlock'
toIRFuncBody (Sol.Block ss) vis (TransformationOnBlock prepends appends) (FuncRetTransResult _ ort rn) = do
  stmts <- mapM _toIR ss

  -- let hasRetStmt =
  --       not (null stmts)
  --         && case last stmts of
  --           Just (ReturnStmt _) -> True
  --           _ -> False

  let midReturn = hasMiddleReturn ss
  let lastReturn = hasLastReturn ss
  traceShowM ("function returnExistsInStmt " ++ show midReturn ++ show lastReturn)

  let stmts' = case (vis == Public, lastReturn, midReturn) of
        (True, True, False) -> case last stmts of
          Just (ReturnStmt r) -> case ort of
            -- use `require(boolExpr);` to replace `return boolExpr;`
            Just (ElementaryType IR.Bool) -> init stmts ++ [Just $ RequireStmt r]
            -- use `require(nonBoolExpr == injectedParamName);` to replace `return nonBoolExpr;`
            _ ->
              let e = fromMaybe (IR.Identifier "retVal") rn
               in init stmts ++ [Just $ requireEqualStmt r e]
          _ -> error "last statement is not return statement"
        (True, False, False) ->
          stmts ++ case rn of
            Just r -> [Just $ requireEqualStmt (IdentifierExpr r) $ mirror rn]
            _ -> [Just $ RequireStmt trueExpr]
        (False, False, False) -> case rn of
          -- append `return returnName;`
          Just n -> stmts ++ [Just $ ReturnStmt $ IdentifierExpr n]
          -- append `return true;`
          _ -> stmts ++ [Just $ ReturnStmt trueExpr]
        _ -> stmts

  let stmts'' = map Just prepends ++ init stmts' ++ map Just appends ++ [last stmts']

  let stmts''' = case reverse stmts'' of
        -- ignore the last `require(true)` if the penultimate is already a `require` stmt
        (Just (RequireStmt (LiteralExpr (BoolLiteral True)))) : (Just (RequireStmt _)) : _ -> init stmts''
        _ -> stmts''

  return $ Just $ IR.Block $ catMaybes stmts'''

mirror :: IIdentifier' -> IIdentifier
mirror (Just (IR.Identifier i)) = IR.Identifier ("_" ++ i)
mirror _ = error "try to mirror an identifier from nothing"

requireEqualStmt :: IExpression -> IIdentifier -> IStatement
requireEqualStmt e i = RequireStmt $ BinaryExpr IR.Equal e $ IdentifierExpr i

trueExpr :: IExpression
trueExpr = LiteralExpr $ BoolLiteral True

mergeTransOnBlock :: TransformationOnBlock -> TransformationOnBlock -> TransformationOnBlock
mergeTransOnBlock (TransformationOnBlock preA appA) (TransformationOnBlock preB appB) =
  TransformationOnBlock (preA ++ preB) (appA ++ appB)

declareLocalVarStmt :: IParam' -> [IStatement]
declareLocalVarStmt Nothing = []
declareLocalVarStmt (Just p@(IR.Param (IR.ElementaryType IR.Bool) _)) = [IR.DeclareStmt [Just p] [LiteralExpr (IR.BoolLiteral False)]]
declareLocalVarStmt (Just p@(IR.Param (IR.ElementaryType IR.Int) _)) = [IR.DeclareStmt [Just p] [LiteralExpr (IR.IntLiteral False 0)]]
declareLocalVarStmt (Just p@(IR.Param (IR.ElementaryType IR.Bytes) _)) = [IR.DeclareStmt [Just p] [LiteralExpr (IR.BytesLiteral [])]]
declareLocalVarStmt (Just (IR.Param t _)) = error $ "unimpmented init statement for type `" ++ show t ++ "`"

-- transformations for functions that need access preimage
transForPreimageFunc :: ([IParam], TransformationOnBlock)
transForPreimageFunc =
  ( [IR.Param (BuiltinType "SigHashPreimage") $ IR.ReservedId varTxPreimage],
    TransformationOnBlock
      []
      -- appends statements
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
        -- add `require(hash256(output) == SigHash.hashOutputs(txPreimage));`
        IR.RequireStmt $
          IR.BinaryExpr
            IR.Equal
            (IR.FunctionCallExpr (IdentifierExpr (IR.ReservedId funcHash256)) [IdentifierExpr (IR.ReservedId varOutput)])
            $ IR.FunctionCallExpr
              (IR.MemberAccessExpr (IdentifierExpr (IR.ReservedId libSigHash)) (IR.Identifier "hashOutputs"))
              [IdentifierExpr (IR.ReservedId varTxPreimage)]
      ]
  )

-- transformations for functions that uses `msg.sender`
transForFuncWithMsgSender :: ([IParam], TransformationOnBlock)
transForFuncWithMsgSender =
  ( [ IR.Param (BuiltinType "Sig") $ IR.ReservedId varSig,
      IR.Param (BuiltinType "PubKey") $ IR.ReservedId varPubKey
    ],
    TransformationOnBlock
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
transForFuncWithMsgValue :: ([IParam], TransformationOnBlock)
transForFuncWithMsgValue =
  ( [],
    -- int msgValue = SigHash.value(txPreimage);
    TransformationOnBlock
      [
        IR.DeclareStmt
          [Just $ IR.Param (ElementaryType IR.Int) (IR.ReservedId varMsgValue)]
          [IR.FunctionCallExpr
              (IR.MemberAccessExpr (IdentifierExpr (IR.ReservedId libSigHash)) (IR.Identifier "value"))
              [IdentifierExpr (IR.ReservedId varTxPreimage)]]
      ]
      []
  )

msgSenderExpr :: Sol.Expression
msgSenderExpr = Sol.MemberAccess (Sol.Literal (PrimaryExpressionIdentifier (Sol.Identifier "msg"))) (Sol.Identifier "sender")

msgValueExpr :: Sol.Expression
msgValueExpr = Sol.MemberAccess (Sol.Literal (PrimaryExpressionIdentifier (Sol.Identifier "msg"))) (Sol.Identifier "value")


toIRConstructorParams :: ParameterList -> [FunctionDefinitionTag] -> Block -> Transformation (IParamList', TransformationOnBlock)
toIRConstructorParams (ParameterList pl) _ funcBlk = do
  params <- mapM _toIR pl

  let extraParams0 = []

  let blkT0 = TransformationOnBlock [] []

  -- for function that uses `msg.sender`
  let (extraParams1, blkT1) =
        if exprExistsInStmt msgSenderExpr (Sol.BlockStatement funcBlk)
          then
            let (ps, blkT_) = transForConstructorWithMsgSender in (map Just ps ++ extraParams0, mergeTransOnBlock blkT0 blkT_)
          else (extraParams0, blkT0)

  let params' = sequence $ params ++ extraParams1

  return (IR.ParamList <$> params', blkT1)


toIRConstructorBody :: Block  -> TransformationOnBlock -> Transformation IBlock'
toIRConstructorBody (Sol.Block ss) (TransformationOnBlock prepends appends) = do
  stmts' <- mapM _toIR ss
  let stmts'' = map Just prepends ++ stmts' ++ map Just appends
  return $ Just $ IR.Block $ catMaybes stmts''



-- transformations for constructor that uses `msg.sender`
transForConstructorWithMsgSender :: ([IR.IParam], TransformationOnBlock)
transForConstructorWithMsgSender =
  ( [ IR.Param (ElementaryType Address) $ IR.ReservedId varMsgSender],
    TransformationOnBlock
      [] -- prepends
      [] -- appends
  )

-- transformations for constructor that uses `msg.value`
transForConstructorWithMsgValue :: ([IR.IParam], TransformationOnBlock)
transForConstructorWithMsgValue =
  ( [ IR.Param (ElementaryType Int) $ IR.ReservedId varMsgValue],
    TransformationOnBlock
      [] -- prepends
      [] -- appends
  )



