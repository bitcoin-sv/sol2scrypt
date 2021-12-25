{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Function where

import Data.Maybe
import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Identifier ()
import IR.Transformations.Sol2IR.Statement ()
import IR.Transformations.Sol2IR.Type ()
import IR.Transformations.Sol2IR.Variable ()
import Solidity.Spec as Sol

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
    vis <- toIRFuncVis tags
    retTransResult <- toIRFuncRet vis maybeRets
    (ps, blkTfromParam) <- toIRFuncParams pl tags retTransResult vis
    body <- toIRFuncBody block vis blkTfromParam retTransResult
    return $ Function (IR.Identifier fn) <$> ps <*> body <*> targetType retTransResult <*> Just vis
  _toIR _ = return Nothing

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

toIRFuncParams :: ParameterList -> [FunctionDefinitionTag] -> FuncRetTransResult -> IVisibility -> Transformation (IParamList', Maybe TransformationOnBlock)
toIRFuncParams (ParameterList pl) tags (FuncRetTransResult rt ort rn) vis = do
  params <- mapM _toIR pl
  -- mirror parameter of the named return param whose type has been changed (due to in public fucntion)
  let extraParams = [IR.Param <$> ort <*> Just (mirror rn') | isJust ort && ort /= rt]
                    where rn' = Just $ fromMaybe (IR.Identifier "retVal") rn

  let needPreimageParam
        | FunctionDefinitionTagStateMutability Pure `elem` tags = False -- pure function do ont need preimage
        | vis == IR.Public = True
        | otherwise = False
  let (extraParams', blkT') =
        if needPreimageParam
          then let (ps, blkT) = transForStateAccessableFunc in (extraParams ++ map Just ps, Just blkT)
          else (extraParams, Nothing)
  let params' = params ++ extraParams'
  let pl' = IR.ParamList <$> sequence params'
  return (pl', blkT')

toIRFuncBody :: Block -> IVisibility -> Maybe TransformationOnBlock -> FuncRetTransResult -> Transformation IBlock'
toIRFuncBody (Sol.Block ss) vis blkT (FuncRetTransResult _ ort rn) = do
  stmts <- mapM _toIR ss
  let hasRetStmt =
        not (null stmts)
          && case last stmts of
            Just (ReturnStmt _) -> True
            _ -> False

  let stmts' = case (vis == Public, hasRetStmt) of
        (True, True) -> case last stmts of
          Just (ReturnStmt r) -> case ort of
            -- use `require(boolExpr);` to replace `return boolExpr;`
            Just (ElementaryType IR.Bool) -> init stmts ++ [Just $ RequireStmt r]
            -- use `require(nonBoolExpr == injectedParamName);` to replace `return nonBoolExpr;`
            _ -> init stmts ++ [Just $ requireEqualStmt r $ mirror $ Just $ fromMaybe (IR.Identifier "retVal") rn]
          _ -> error "last statement is not return statement"
        (True, False) -> case rn of
          -- append `require(returnName == injectedParamName)`
          Just n -> stmts ++ [Just $ requireEqualStmt (IdentifierExpr n) $ mirror rn]
          -- append `require(true);`
          _ -> stmts ++ [Just $ RequireStmt trueExpr]
        (False, False) -> case rn of
          -- append `return returnName;`
          Just n -> stmts ++ [Just $ ReturnStmt $ IdentifierExpr n]
          -- append `return true;`
          _ -> stmts ++ [Just $ ReturnStmt trueExpr]
        _ -> stmts

  let stmts'' = case blkT of
        -- keep return/require statement at the end & apply other prepends/appends
        Just (TransformationOnBlock prepends appends) -> map Just prepends ++ init stmts' ++ map Just appends ++ [last stmts']
        _ -> stmts'

  return $ IR.Block <$> sequence stmts''

mirror :: IIdentifier' -> IIdentifier
mirror (Just (IR.Identifier i)) = IR.Identifier ("_" ++ i)
mirror _ = error "try to mirror an identifier from nothing"

requireEqualStmt :: IExpr -> IIdentifier -> IStatement
requireEqualStmt e i = RequireStmt $ BinaryExpr IR.Equal e $ IdentifierExpr i

trueExpr :: IExpr
trueExpr = LiteralExpr $ BoolLiteral True

transForStateAccessableFunc :: ([IParam], TransformationOnBlock)
transForStateAccessableFunc =
  ( [IR.Param (BuiltinType "SigHashPreimage") $ IR.Identifier "txPreimage"],
    TransformationOnBlock
      [] -- TODO: add stmts
      []
  )
