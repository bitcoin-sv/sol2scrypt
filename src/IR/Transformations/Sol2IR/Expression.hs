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
import IR.Transformations.Sol2IR.Identifier (maybeStateVarId)
import Numeric
import Solidity.Spec as Sol
import Utils

-- only used by Transfer array sub. eg. int[20]
instance ToIRTransformable (Sol.Expression SourceRange) Int where
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteral (NumberLiteralDec n _) _))) = return (fst $ head $ readDec n)
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteral (NumberLiteralHex n _) _))) = return (fst $ head $ readHex n)
  _toIR e = error $ "unsupported expression to Integer : `" ++ show e ++ "`"

instance ToIRTransformable (Maybe (Sol.Expression SourceRange)) Int where
  _toIR (Just e) = _toIR e
  _toIR e = error $ "unsupported expression to Integer : `" ++ show e ++ "`"

instance ToIRTransformable (Maybe (Sol.Expression SourceRange)) IExpression' where
  _toIR (Just e) = _toIR e
  _toIR Nothing = return Nothing

instance ToIRTransformable (Sol.Expression SourceRange) IExpression' where
  _toIR (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral b _))) =
    return $ Just $ LiteralExpr $ IR.BoolLiteral ("true" == toLower b)
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteral (NumberLiteralHex n Nothing) _ ))) =
    return $ Just $ LiteralExpr $ IR.IntLiteral True (fst $ head $ readHex n)
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteral (NumberLiteralDec n Nothing) _ ))) =
    return $ Just $ LiteralExpr $ IR.IntLiteral False (fst $ head $ readDec n)
  _toIR (Literal (PrimaryExpressionHexLiteral (HexLiteral h _))) =
    return $ Just $ LiteralExpr $ IR.BytesLiteral $ parseHex h
  _toIR (Literal (PrimaryExpressionStringLiteral (Sol.StringLiteral s _))) =
    return $ Just $ LiteralExpr $ IR.StringLiteral s
  _toIR (Literal (PrimaryExpressionIdentifier i)) = do
    i' <- _toIR i
    i'' <- maybeStateVarId i'
    return $ IdentifierExpr <$> i''
  _toIR (Unary opStr e _) = do
    e' <- _toIR e
    let allIncDecOps = ["++", "--", "()++", "()--"]
    when (opStr `elem` allIncDecOps) $
      checkLHSmapExpr e'
    return $ transformUnaryExpr opStr e'
  _toIR e@(Binary "[]" e1 e2 _) = do
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
          Just (Symbol _ (Mapping _ vt) _) -> do
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
  _toIR (Binary opStr e1 e2 _) = do
    e1' <- _toIR e1
    e2' <- _toIR e2
    -- TODO: check e1's type, it should be `bytes` for bytesOnlyAssignOps
    let bytesOnlyAssignOps = ["%=", "&=", "|=", "^=", "<<=", ">>="]
        assignOps = ["+=", "-=", "*=", "/="] ++ bytesOnlyAssignOps
    when (opStr `elem` assignOps) $
      checkLHSmapExpr e1'
    return $ BinaryExpr (str2BinaryOp opStr) <$> e1' <*> e2'
  _toIR (Ternary _ e1 e2 e3 _) = do
    e1' <- _toIR e1
    e2' <- _toIR e2
    e3' <- _toIR e3
    return $ TernaryExpr <$> e1' <*> e2' <*> e3'
  _toIR (Sol.MemberAccess e i _) = do
      case (e, i) of
        (Literal (PrimaryExpressionIdentifier (Sol.Identifier ne _)), Sol.Identifier ni _) -> do
          case (ne, ni) of
            ("msg", "sender") ->
              return $ Just $ IR.IdentifierExpr (IR.ReservedId varMsgSender)
            ("msg", "value") ->
              return $ Just $ IR.IdentifierExpr (IR.ReservedId varMsgValue)
            _ -> do
              if isMemberAccessSupported (ne, ni)
                then do
                  e' <- _toIR e
                  i' <- _toIR i
                  return $ IR.MemberAccessExpr <$> e' <*> i'
                else error $ "unsupported expression: `" ++ ne ++ "." ++ ni ++ "`"
                  
        _ -> do
              e' <- _toIR e
              i' <- _toIR i
              return $ IR.MemberAccessExpr <$> e' <*> i'
  _toIR (FunctionCallExpressionList fe pl _) = do
    case fe of
      (Literal (PrimaryExpressionIdentifier (Sol.Identifier fn _))) -> do
              if isBuiltInFnSupported fn
                then do
                  if shouldIgnoreBuiltInTypes fn
                  then case pl of
                      Just (ExpressionList [p]) -> _toIR p
                      _ -> error $ "unsupported function call : `" ++ fn ++ "`"
                  else do
                    fe' <- _toIR fe
                    ps' <- case pl of
                      Nothing -> return []
                      Just (ExpressionList ps) -> mapM _toIR ps
                    return $ FunctionCallExpr <$> fe' <*> sequence ps'
                else error $ "unsupported function call : `" ++ fn ++ "`"
      (New (TypeNameElementaryTypeName (ElementaryTypeName (BytesType Nothing) a) _) _) -> do -- eg. transpile Solidity `new bytes(3)` to `num2bin(0, 3)`
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
  _toIR (Literal (PrimaryExpressionTupleExpression (SquareBrackets array _))) = do
    array' <- mapM _toIR array
    return $ Just $ ArrayLiteralExpr $ catMaybes array'
  _toIR e = error $ "unsupported expression : `" ++ headWord (show e) ++ "`"


transformUnaryExpr :: String -> IExpression' -> IExpression'
transformUnaryExpr opStr e' =
  case opStr of
    "-" -> UnaryExpr Negate <$> e'
    "()" -> ParensExpr <$> e'
    "()++" -> UnaryExpr PostIncrement <$> e'
    "++" -> UnaryExpr PreIncrement <$> e'
    "()--" -> UnaryExpr PostDecrement <$> e'
    "--" -> UnaryExpr PreDecrement <$> e'
    "!" -> UnaryExpr Not <$> e'
    s -> error $ "unsupported unary operator `" ++ s ++ "`"

str2BinaryOp :: String -> IBinaryOp
str2BinaryOp "+" = Add
str2BinaryOp "-" = Sub
str2BinaryOp "*" = Mul
str2BinaryOp "/" = Div
str2BinaryOp "%" = Mod
str2BinaryOp "+=" = AddAssign
str2BinaryOp "-=" = SubAssign
str2BinaryOp "*=" = MulAssign
str2BinaryOp "/=" = DivAssign
str2BinaryOp "%=" = ModAssign
str2BinaryOp "&=" = AndAssign -- only works on bytes in scrypt
str2BinaryOp "|=" = OrAssign -- only works on bytes in scrypt
str2BinaryOp "^=" = XorAssign -- only works on bytes in scrypt
str2BinaryOp "<<=" = LShiftAssign -- only works on bytes in scrypt
str2BinaryOp ">>=" = RShiftAssign -- only works on bytes in scrypt
str2BinaryOp "==" = IR.Equal
str2BinaryOp "!=" = Neq
str2BinaryOp "<" = LessThan
str2BinaryOp "<=" = LessThanOrEqual
str2BinaryOp ">" = GreaterThan
str2BinaryOp ">=" = GreaterThanOrEqual
str2BinaryOp "&&" = BoolAnd
str2BinaryOp "||" = BoolOr
str2BinaryOp "[]" = Index
str2BinaryOp s = error $ "unsupported binary operator `" ++ s ++ "`"

-- give an expression a var name which can be used in transformation, for example, declare a var for the expression.
toExprName :: IExpression -> ExprName
toExprName (LiteralExpr (BoolLiteral b)) = show b
toExprName (LiteralExpr (IntLiteral _ i)) = show i
toExprName (LiteralExpr (BytesLiteral bytes)) = concatMap showHexWithPadded bytes
toExprName (IdentifierExpr (IR.Identifier n)) = replaceDotWithUnderscore n
toExprName (IdentifierExpr (IR.ReservedId n)) = replaceDotWithUnderscore n
toExprName (BinaryExpr _ le re) = toExprName le ++ "_" ++ toExprName re
toExprName (StructLiteralExpr es) = intercalate "_" $ map toExprName es
toExprName e = error $ "the expr is not supported in #toExprName: " ++ show e

incExprCounter :: MappingExprCounter -> IExpression' -> IExpression' -> IType -> MappingExprCounter
incExprCounter ec (Just mapping) (Just key) et = Map.insert en (MECEntry et mapping key cnt updated) ec
  where
    en = toExprName $ BinaryExpr Index mapping key
    entry = Map.lookup en ec
    cnt = maybe 0 exprCnt entry
    updated = maybe False entryUpdated entry
incExprCounter ec _ _ _ = ec

-- get base identifier for binary index expression: `a[x][y]` -> `a`
baseIdOfBinaryIndexExpr :: Expression SourceRange -> Transformation IIdentifier'
baseIdOfBinaryIndexExpr (Binary "[]" e@(Binary "[]" _ _ _) _ _) = baseIdOfBinaryIndexExpr e
baseIdOfBinaryIndexExpr (Binary "[]" (Literal (PrimaryExpressionIdentifier i)) _ _) = do
  i' <- _toIR i
  maybeStateVarId i'
baseIdOfBinaryIndexExpr _ = return Nothing

-- get expression for the key of mapping
keyExprOfMapping :: Expression SourceRange -> [Expression SourceRange] -> Transformation IExpression'
keyExprOfMapping (Binary "[]" e@(Binary "[]" _ _ _) ke _) keys = keyExprOfMapping e $ ke : keys
keyExprOfMapping (Binary "[]" _ ke _) keys =
  if null keys
    then do
      e <- _toIR ke
      let ke' = case toExprName <$> e of
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
    n = replaceDotWithUnderscore . toExprName <$> e
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
indexExprOfMapping :: IExpression' -> String -> IExpression'
indexExprOfMapping e@(Just (BinaryExpr Index _ _)) postfix = IdentifierExpr <$> (IR.Identifier <$> indexNameOfMapping e postfix)
indexExprOfMapping _ _ = Nothing

-- check left-hand-side map expression
checkLHSmapExpr :: IExpression' -> Transformation ()
checkLHSmapExpr e = do
  -- set updated flag for mapping type LHS
  mapCounter <- gets stateInFuncMappingCounter
  let mapName = toExprName <$> e
  case join $ Map.lookup <$> mapName <*> Just mapCounter of
    Just entry -> do
      let mapCounter' = Map.insert (fromJust mapName) (entry {entryUpdated = True}) mapCounter
      modify $ \s -> s {stateInFuncMappingCounter = mapCounter'}
    _ -> return ()

-- some solidity buildin functions are not supported
isBuiltInFnSupported :: String -> Bool
isBuiltInFnSupported fn = fn `notElem` ["assert", "keccak256", "ecrecover", "addmod", "mulmod", "revert", "selfdestruct", "type"]

-- some solidity buildin member access are not supported
isMemberAccessSupported :: (String, String) -> Bool
isMemberAccessSupported (e, i) = case (e, i) of
  ("msg", "sig") -> False 
  ("msg", "data") -> False
  ("block", _) -> False
  ("tx", _) -> False
  ("abi", _) -> False
  _ -> True

-- cast functions in solidity need to be ignored
shouldIgnoreBuiltInTypes :: String -> Bool
shouldIgnoreBuiltInTypes fn = fn `elem` ["string", "uint", "uint8", "uint16", "uint24", "uint32", "uint40", "uint48", "uint56", "uint64",
  "uint72", "uint88", "uint96", "uint104", "uint112", "uint120", "uint128", "uint136", "uint144", "uint152", "uint160", "uint168", "uint176",
  "uint184", "uint192", "uint200", "uint208", "uint216", "uint224", "uint232", "uint240", "uint248", "uint256",
  "int", "int8", "int16", "int24", "int32", "int40", "int48", "int56", "int64",
  "int72", "int88", "int96", "int104", "int112", "int120", "int128", "int136", "int144", "int152", "int160", "int168", "int176",
  "int184", "int192", "int200", "int208", "int216", "int224", "int232", "int240", "int248", "int256", "bool",
  "bytes", "bytes1", "bytes2", "bytes3", "bytes4", "bytes5", "bytes6", "bytes7", "bytes8", "bytes9", "bytes10", "bytes11", "bytes12",
  "bytes13", "bytes14", "bytes15", "bytes16", "bytes17", "bytes18", "bytes19", "bytes20", "bytes21", "bytes22", "bytes23", "bytes24",
  "bytes25", "bytes26", "bytes27", "bytes28", "bytes29", "bytes30", "bytes31", "bytes32"]

