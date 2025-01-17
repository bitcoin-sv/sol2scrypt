{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Expression where

import Control.Monad.State
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Identifier
import Numeric
import Solidity.Parser (display)
import Solidity.Spec as Sol
import Utils

-- only used by Transfer array sub. eg. int[20]
instance ToIRTransformable (Sol.Expression SourceRange) Int where
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteral (NumberLiteralDec n _) _))) = return (fst $ head $ readDec n)
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteral (NumberLiteralHex n _) _))) = return (fst $ head $ readHex n)
  _toIR e = reportError ("unsupported number literal expression: `" ++ display e ++ "`") (ann e) >> return 0

instance ToIRTransformable (Maybe (Sol.Expression SourceRange)) IExpression' where
  _toIR (Just e) = _toIR e
  _toIR Nothing = return Nothing

instance ToIRTransformable (Sol.Expression SourceRange) IExpression' where
  _toIR (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral b _))) =
    return $ Just $ LiteralExpr $ IR.BoolLiteral ("true" == toLower b)
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteral (NumberLiteralHex n unit) a))) =
    if isJust unit
      then reportError ("unsupported number unit: `" ++ display (fromJust unit) ++ "`") a >> return Nothing
      else return $ Just $ LiteralExpr $ IR.IntLiteral True (fst $ head $ readHex n)
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteral (NumberLiteralDec n unit) a))) =
    if isJust unit
      then reportError ("unsupported number unit: `" ++ display (fromJust unit) ++ "`") a >> return Nothing
      else return $ Just $ LiteralExpr $ IR.IntLiteral False (fst $ head $ readDec n)
  _toIR (Literal (PrimaryExpressionHexLiteral (HexLiteral h _))) =
    return $ Just $ LiteralExpr $ IR.BytesLiteral $ parseHex h
  _toIR (Literal (PrimaryExpressionStringLiteral (Sol.StringLiteral s _))) =
    return $ Just $ LiteralExpr $ IR.StringLiteral s
  _toIR (Literal (PrimaryExpressionIdentifier i)) = do
    i' <- _toIR i
    i'' <- maybeStateVarId i'
    return $ IdentifierExpr <$> i''
  _toIR (Literal (PrimaryExpressionTupleExpression (SquareBrackets array _))) = do
    array' <- mapM _toIR array
    return $ Just $ ArrayLiteralExpr $ catMaybes array'
  _toIR ((Literal (PrimaryExpressionTupleExpression e))) = reportError ("unsupported tuple expression: `" ++ display e ++ "`") (ann e) >> return Nothing
  _toIR (Literal (PrimaryExpressionElementaryTypeNameExpression e)) = reportError ("unsupported elementary type name expression: `" ++ display e ++ "`") (ann e) >> return Nothing
  _toIR (Unary op@(Operator opStr _) e _) = do
    e' <- _toIR e
    let allIncDecOps = ["++", "--", "()++", "()--"]
    when (opStr `elem` allIncDecOps) $
      checkLHSmapExpr e'
    transformUnaryExpr op e'
  _toIR e@(Binary (Operator "[]" _) e1 e2 _) = do
    let arrayIndexAccess = \arr idx -> do
          arr' <- _toIR arr
          idx' <- _toIR idx
          return $ BinaryExpr Index <$> arr' <*> idx'
    baseId <- baseIdOfBinaryIndexExpr e
    case baseId of
      -- due to the lack of semantics, only `IdentifierExpr` can get resolved type here.
      Just i -> do
        i' <- lookupSym $ case i of
          IR.Identifier n -> IR.Identifier $ stripThis n
          ReservedId n -> ReservedId $ stripThis n
        case i' of
          -- mapping-typed var access
          Just (Symbol _ (Mapping _ vt) _ _ _) -> do
            mc <- gets stateInFuncMappingCounter
            let mapExpr = Just $ IdentifierExpr i
            keyExpr <- keyExprOfMapping e []
            modify $ \s -> s {stateInFuncMappingCounter = incExprCounter mc mapExpr keyExpr vt}
            -- transpile to a value expression
            let e' = BinaryExpr Index <$> mapExpr <*> keyExpr
            return $ valueExprOfMapping e' "" -- with empty postfix
          _ -> arrayIndexAccess e1 e2
      -- other exprs whose type is `Mapping` will not be correctly transpiled below due to the same above.
      _ -> arrayIndexAccess e1 e2
  _toIR (Binary operator@(Operator opStr _) e1 e2 _) = do
    e1' <- _toIR e1
    e2' <- _toIR e2
    -- TODO: check e1's type, it should be `bytes` for bytesOnlyAssignOps
    let bytesOnlyAssignOps = ["%=", "&=", "|=", "^="]
        assignOps = ["+=", "-=", "*=", "/=", "<<=", ">>="] ++ bytesOnlyAssignOps
    when (opStr `elem` assignOps) $
      checkLHSmapExpr e1'
    transformBinraryExpr operator e1' e2'
  _toIR (Ternary _ e1 e2 e3 _) = do
    e1' <- _toIR e1
    e2' <- _toIR e2
    e3' <- _toIR e3
    return $ TernaryExpr <$> e1' <*> e2' <*> e3'
  _toIR (Sol.MemberAccess e i a) = do
    case (e, i) of
      (Literal (PrimaryExpressionIdentifier (Sol.Identifier ne _)), Sol.Identifier ni _) -> do
        case (ne, ni) of
          ("msg", "sender") ->
            return $ Just $ IR.IdentifierExpr (IR.ReservedId varMsgSender)
          ("msg", "value") ->
            return $ Just $ IR.IdentifierExpr (IR.ReservedId varMsgValue)
          _ -> do
            supported <- isMemberAccessSupported (ne, ni)
            if supported
              then do
                e' <- _toIR e
                i' <- _toIR i
                return $ IR.MemberAccessExpr <$> e' <*> i'
              else reportError ("unsupported expression: `" ++ ne ++ "." ++ ni ++ "`") a >> return Nothing
      _ -> do
        e' <- _toIR e
        i' <- _toIR i
        return $ IR.MemberAccessExpr <$> e' <*> i'
  _toIR (FunctionCallExpressionList fe pl fa) = do
    case fe of
      (Literal (PrimaryExpressionIdentifier i@(Sol.Identifier fn _))) -> do
        if isBuiltInFnSupported fn
          then do
            st' <- lookupStruct fn
            if shouldIgnoreBuiltInTypes fn
              then case pl of
                Just (ExpressionList [p]) -> _toIR p
                _ -> reportError ("unsupported function call: `" ++ fn ++ "`") fa >> return Nothing
              else -- transpile sol `address(0)` to sCrypt `Ripemd160(b'')`

                if isAddress0 fe pl
                  then return $ Just $ FunctionCallExpr (IdentifierExpr (IR.ReservedId "Ripemd160")) [LiteralExpr $ BytesLiteral [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]
                  else
                    if isJust st'
                      then do
                        ps' <- case pl of
                          Nothing -> return []
                          Just (ExpressionList ps) -> mapM _toIR ps
                        return $ StructLiteralExpr <$> sequence ps'
                      else do
                        i' <- _toIR i
                        i'' <- maybeMemberFunctionCall i'
                        ps' <- case pl of
                          Nothing -> return []
                          Just (ExpressionList ps) -> mapM _toIR ps
                        return $ FunctionCallExpr <$> (IdentifierExpr <$> i'') <*> sequence ps'
          else reportError ("unsupported function call: `" ++ fn ++ "`") fa >> return Nothing
      (New (TypeNameElementaryTypeName (ElementaryTypeName (BytesType Nothing) a) _) _) -> do
        -- eg. transpile Solidity `new bytes(3)` to `num2bin(0, 3)`
        ps' <- case pl of
          Nothing -> return []
          Just (ExpressionList ps) -> mapM _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteral (NumberLiteralDec "0" Nothing) a)) : ps)
        return $ FunctionCallExpr (IR.IdentifierExpr (IR.Identifier "num2bin")) <$> sequence ps'
      _ -> do
        fe' <- _toIR fe
        ps' <- case pl of
          Nothing -> return []
          Just (ExpressionList ps) -> mapM _toIR ps
        return $ FunctionCallExpr <$> fe' <*> sequence ps'
  _toIR (FunctionCallNameValueList fe pl fa) = do
    case fe of
      (Literal (PrimaryExpressionIdentifier (Sol.Identifier fn _))) -> do
        if isBuiltInFnSupported fn
          then do
            st' <- lookupStruct fn
            if shouldIgnoreBuiltInTypes fn
              then case pl of
                Just (NameValueList [nv]) -> _toIR (snd nv)
                _ -> reportError ("unsupported function call: `" ++ fn ++ "`") fa >> return Nothing
              else
                if isJust st'
                  then do
                    ps' <- case pl of
                      Nothing -> return []
                      Just (NameValueList ps) -> mapM (_toIR . snd) ps
                    return $ StructLiteralExpr <$> sequence ps'
                  else do
                    fe' <- _toIR fe
                    ps' <- case pl of
                      Nothing -> return []
                      Just (NameValueList ps) -> mapM (_toIR . snd) ps
                    return $ FunctionCallExpr <$> fe' <*> sequence ps'
          else reportError ("unsupported function call: `" ++ fn ++ "`") fa >> return Nothing
      _ -> do
        fe' <- _toIR fe
        ps' <- case pl of
          Nothing -> return []
          Just (NameValueList ps) -> mapM (_toIR . snd) ps
        return $ FunctionCallExpr <$> fe' <*> sequence ps'
  _toIR (New _ a) = reportError "unsupported new expression" a >> return Nothing

isBytes :: IType -> Bool
isBytes (ElementaryType Bytes) = True
isBytes _ = False

transformUnaryExpr :: Operator SourceRange -> IExpression' -> Transformation IExpression'
transformUnaryExpr (Operator opStr a) e' =
  case opStr of
    "-" -> return $ UnaryExpr Negate <$> e'
    "()" -> return $ ParensExpr <$> e'
    "()++" -> return $ UnaryExpr PostIncrement <$> e'
    "++" -> return $ UnaryExpr PreIncrement <$> e'
    "()--" -> return $ UnaryExpr PostDecrement <$> e'
    "--" -> return $ UnaryExpr PreDecrement <$> e'
    "!" -> return $ UnaryExpr Not <$> e'
    "~" -> return $ UnaryExpr Invert <$> e'
    s -> reportError ("unsupported unary operator: `" ++ s ++ "`") a >> return Nothing

transformBinraryExpr :: Operator SourceRange -> IExpression' -> IExpression' -> Transformation IExpression'
transformBinraryExpr (Operator opStr a) e1' e2' = do
  op <- str2BinaryOp opStr a
  return $ BinaryExpr <$> op <*> e1' <*> e2'

str2BinaryOp :: String -> SourceRange -> Transformation (Maybe IBinaryOp)
str2BinaryOp "+" _ = return $ Just Add
str2BinaryOp "-" _ = return $ Just Sub
str2BinaryOp "*" _ = return $ Just Mul
str2BinaryOp "/" _ = return $ Just Div
str2BinaryOp "%" _ = return $ Just Mod
str2BinaryOp "+=" _ = return $ Just AddAssign
str2BinaryOp "-=" _ = return $ Just SubAssign
str2BinaryOp "*=" _ = return $ Just MulAssign
str2BinaryOp "/=" _ = return $ Just DivAssign
str2BinaryOp "%=" _ = return $ Just ModAssign
str2BinaryOp "==" _ = return $ Just IR.Equal
str2BinaryOp "!=" _ = return $ Just Neq
str2BinaryOp "<" _ = return $ Just LessThan
str2BinaryOp "<=" _ = return $ Just LessThanOrEqual
str2BinaryOp ">" _ = return $ Just GreaterThan
str2BinaryOp ">=" _ = return $ Just GreaterThanOrEqual
str2BinaryOp "&&" _ = return $ Just BoolAnd
str2BinaryOp "||" _ = return $ Just BoolOr
str2BinaryOp "[]" _ = return $ Just Index
str2BinaryOp "&" _ = return $ Just And
str2BinaryOp "|" _ = return $ Just Or
str2BinaryOp "^" _ = return $ Just Xor
str2BinaryOp "&=" _ = return $ Just AndAssign
str2BinaryOp "|=" _ = return $ Just OrAssign
str2BinaryOp "^=" _ = return $ Just XorAssign
str2BinaryOp "<<=" _ = return $ Just LShiftAssign
str2BinaryOp ">>=" _ = return $ Just RShiftAssign
str2BinaryOp "<<" _ = return $ Just LShift
str2BinaryOp ">>" _ = return $ Just RShift
str2BinaryOp s a = reportError ("unsupported binary operator: `" ++ s ++ "`") a >> return Nothing

-- give an expression a var name which can be used in transformation, for example, declare a var for the expression.
toExprName :: Bool -> IExpression -> ExprName
toExprName _ (LiteralExpr (BoolLiteral b)) = show b
toExprName _ (LiteralExpr (IntLiteral _ i)) = show i
toExprName _ (LiteralExpr (BytesLiteral bytes)) = concatMap showHexWithPadded bytes
toExprName keepDot (IdentifierExpr (IR.Identifier n)) = if keepDot then n else replaceDotWithUnderscore $ stripThis n
toExprName keepDot (IdentifierExpr (IR.ReservedId n)) = if keepDot then n else replaceDotWithUnderscore $ stripThis n
toExprName _ (BinaryExpr _ le re) = toExprName False le ++ "_" ++ toExprName False re
toExprName _ (StructLiteralExpr es) = intercalate "_" $ map (toExprName False) es
toExprName keepDot (MemberAccessExpr e i@(IR.Identifier _)) = toExprName keepDot e ++ (if keepDot then "." else "_") ++ toExprName keepDot (IdentifierExpr i)
toExprName _ e = error $ "the expr is not supported in #toExprName: " ++ show e

incExprCounter :: MappingExprCounter -> IExpression' -> IExpression' -> IType -> MappingExprCounter
incExprCounter ec (Just mapping) (Just key) et = Map.insert en (MECEntry et mapping key cnt updated i) ec
  where
    en = toExprName True $ BinaryExpr Index mapping key
    entry = Map.lookup en ec
    cnt = maybe 0 exprCnt entry
    updated = maybe False entryUpdated entry
    i = maybe (Map.size ec) index entry
incExprCounter ec _ _ _ = ec

-- get base identifier for binary index expression: `a[x][y]` -> `a`
baseIdOfBinaryIndexExpr :: Expression SourceRange -> Transformation IIdentifier'
baseIdOfBinaryIndexExpr (Binary (Operator "[]" _) e@(Binary (Operator "[]" _) _ _ _) _ _) = baseIdOfBinaryIndexExpr e
baseIdOfBinaryIndexExpr (Binary (Operator "[]" _) (Literal (PrimaryExpressionIdentifier i)) _ _) = do
  i' <- _toIR i
  maybeStateVarId i'
baseIdOfBinaryIndexExpr _ = return Nothing

-- get expression for the key of mapping
keyExprOfMapping :: Expression SourceRange -> [Expression SourceRange] -> Transformation IExpression'
keyExprOfMapping (Binary (Operator "[]" _) e@(Binary (Operator "[]" _) _ _ _) ke _) keys = keyExprOfMapping e $ ke : keys
keyExprOfMapping (Binary (Operator "[]" _) _ ke _) keys =
  if null keys
    then do
      e <- _toIR ke
      let ke' = case toExprName True <$> e of
            Just kn -> if kn `elem` reservedNames then Just (IR.ReservedId kn) else Just (IR.Identifier kn)
            _ -> Nothing
      -- use IdentifierExpr for non-nested-mapping key
      return $ IdentifierExpr <$> ke'
    else do
      keys' <- mapM _toIR $ ke : keys
      -- use StructLiteralExpr for nested-mapping keys
      return $ StructLiteralExpr <$> sequence keys'
keyExprOfMapping _ _ = return Nothing

-- name for mapping expression, e.x. use `mapping_key` as name of expr `mapping[key]`
valueNameOfMapping :: IExpression' -> String -> Maybe String
valueNameOfMapping e postfix = n'
  where
    n = toExprName False <$> e
    n' = (++) <$> n <*> Just postfix

-- use identifier expression to replace mapping expression
valueExprOfMapping :: IExpression' -> String -> IExpression'
valueExprOfMapping e@(Just (BinaryExpr Index _ _)) postfix = IdentifierExpr <$> (IR.Identifier <$> valueNameOfMapping e postfix)
valueExprOfMapping _ _ = Nothing

-- index name for mapping expression, e.x. use `mapping_key_index` as index name for expr `mapping[key]`
indexNameOfMapping :: IExpression' -> String -> Maybe String
indexNameOfMapping e postfix = (++) <$> valName <*> Just "_index"
  where
    valName = valueNameOfMapping e postfix

-- corresponding index expression for mapping expression
indexExprOfMapping :: Int -> IExpression'
indexExprOfMapping i = Just $ IdentifierExpr $ IR.Identifier ("i" ++ show i)

-- check left-hand-side map expression
checkLHSmapExpr :: IExpression' -> Transformation ()
checkLHSmapExpr e = do
  -- set updated flag for mapping type LHS
  mapCounter <- gets stateInFuncMappingCounter
  let mapName = toExprName False <$> e
  case join $ Map.lookup <$> mapName <*> Just mapCounter of
    Just entry -> do
      let mapCounter' = Map.insert (fromJust mapName) (entry {entryUpdated = True}) mapCounter
      modify $ \s -> s {stateInFuncMappingCounter = mapCounter'}
    _ -> return ()

-- some solidity buildin functions are not supported
isBuiltInFnSupported :: String -> Bool
isBuiltInFnSupported fn =
  fn
    `notElem` [ "assert",
                "keccak256",
                "ecrecover",
                "addmod",
                "blockhash",
                "mulmod",
                "revert",
                "selfdestruct",
                "type",
                "gasleft",
                "payable"
              ]

-- some solidity buildin member access are not supported
isMemberAccessSupported :: (String, String) -> Transformation Bool
isMemberAccessSupported (e, i) = case (e, i) of
  ("msg", "sig") -> return False
  ("msg", "data") -> return False
  ("msg", "gas") -> return False
  ("block", _) -> return False
  ("tx", _) -> return False
  ("abi", _) -> return False
  (v, "balance") -> do
    sym <- lookupSym $ IR.Identifier v
    return
      ( case sym of
          Nothing -> False
          Just sym' ->
            ( case symbolType sym' of
                ElementaryType Address -> False
                _ -> True
            )
      )
  (_, "transfer") -> return False
  (_, "send") -> return False
  _ -> return True

-- cast functions in solidity need to be ignored
shouldIgnoreBuiltInTypes :: String -> Bool
shouldIgnoreBuiltInTypes fn =
  fn
    `elem` [ "string",
             "uint",
             "uint8",
             "uint16",
             "uint24",
             "uint32",
             "uint40",
             "uint48",
             "uint56",
             "uint64",
             "uint72",
             "uint88",
             "uint96",
             "uint104",
             "uint112",
             "uint120",
             "uint128",
             "uint136",
             "uint144",
             "uint152",
             "uint160",
             "uint168",
             "uint176",
             "uint184",
             "uint192",
             "uint200",
             "uint208",
             "uint216",
             "uint224",
             "uint232",
             "uint240",
             "uint248",
             "uint256",
             "int",
             "int8",
             "int16",
             "int24",
             "int32",
             "int40",
             "int48",
             "int56",
             "int64",
             "int72",
             "int88",
             "int96",
             "int104",
             "int112",
             "int120",
             "int128",
             "int136",
             "int144",
             "int152",
             "int160",
             "int168",
             "int176",
             "int184",
             "int192",
             "int200",
             "int208",
             "int216",
             "int224",
             "int232",
             "int240",
             "int248",
             "int256",
             "bool",
             "bytes",
             "bytes1",
             "bytes2",
             "bytes3",
             "bytes4",
             "bytes5",
             "bytes6",
             "bytes7",
             "bytes8",
             "bytes9",
             "bytes10",
             "bytes11",
             "bytes12",
             "bytes13",
             "bytes14",
             "bytes15",
             "bytes16",
             "bytes17",
             "bytes18",
             "bytes19",
             "bytes20",
             "bytes21",
             "bytes22",
             "bytes23",
             "bytes24",
             "bytes25",
             "bytes26",
             "bytes27",
             "bytes28",
             "bytes29",
             "bytes30",
             "bytes31",
             "bytes32"
           ]

isAddress0 :: Expression SourceRange -> Maybe (ExpressionList a) -> Bool
isAddress0 (Literal (PrimaryExpressionIdentifier (Sol.Identifier "address" _))) (Just (ExpressionList [Literal (PrimaryExpressionNumberLiteral (NumberLiteral (NumberLiteralDec "0" Nothing) _))])) = True
isAddress0 _ _ = False