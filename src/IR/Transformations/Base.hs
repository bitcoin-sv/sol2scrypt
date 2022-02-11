{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Transformations.Base where

import Control.Applicative hiding (Const)
import Control.Monad.State
import qualified Data.Map.Lazy as Map
import Data.Maybe
import IR.Spec as IR
import Solidity.Parser
import Text.Parsec hiding (try, (<|>))

parseIO :: Parseable a => String -> IO a
parseIO solidityCode = either (fail . (parseError ++) . show) return $ parse parser "" solidityCode
  where
    parseError = "Error during parsing of <" ++ solidityCode ++ ">\n"

-----------------  Solidity to IR -----------------
data TransformState = TransformState
  { stateEnv :: Env,
    -- TFStmtWrapper for current statement
    stateWrapperForStmt :: Maybe TFStmtWrapper,
    -- counter for mapping-access exprs in a function, key is the expression name, value is its occurrences count
    stateInFuncMappingCounter :: MappingExprCounter,
    -- have returned previously in current block
    stateReturnedInBlock :: [Bool]
  }
  deriving (Show, Eq, Ord)

type Transformation a = StateT TransformState IO a

class Parseable sol => ToIRTransformable sol ir where
  _toIR :: sol -> Transformation ir

transform2IR :: ToIRTransformable sol ir => TransformState -> sol -> IO ir
transform2IR ts sol = fst <$> runStateT (_toIR sol) ts

type ExprName = String

-- counter for mapping-related expression occurrences, `ExprName` as key
type MappingExprCounter = Map.Map ExprName MECEntry

-- Mapping Expr Counter Entry
data MECEntry = MECEntry
  { -- the expression's type
    exprType :: IType,
    -- mapping instance expr
    mExpr :: IExpression,
    -- key expr
    kExpr :: IExpression,
    -- the expression occurrences count
    exprCnt :: Int
  }
  deriving (Show, Eq, Ord)

-- symbols & enviroment
type SymbolName = IIdentifier

data Symbol = Symbol
  { symbolName :: SymbolName,
    symbolType :: IType,
    symbolStateVar :: Bool
  }
  deriving (Show, Eq, Ord)

type SymbolTable = Map.Map SymbolName Symbol

type Env = [SymbolTable]

addScope :: Env -> Env
addScope scopes = Map.empty : scopes

dropScope :: Env -> Env
dropScope = tail

enterScope :: Transformation ()
enterScope = do
  env <- gets stateEnv
  returned <- gets stateReturnedInBlock
  modify $ \s ->
    s
      { stateEnv = addScope env,
        -- add default value of current block's returned flag
        stateReturnedInBlock = False : returned
      }

leaveScope :: Transformation ()
leaveScope = do
  env <- gets stateEnv
  returned <- gets stateReturnedInBlock
  modify $ \s ->
    s
      { stateEnv = dropScope env,
        stateReturnedInBlock = case returned of
          -- merge current block's returned flag to its outer block's
          cr : outerR : rs -> (cr || outerR) : rs
          -- keep the outermost returned flag
          [cr] -> [cr]
          _ -> []
      }

newtype SymbolTableUpdateError = SymbolTableUpdateError String deriving (Show, Eq, Ord)

addSymbol :: Symbol -> Env -> Either SymbolTableUpdateError Env
addSymbol s [] = addSymbol s [Map.empty]
addSymbol s (scope : scopes)
  | isJust (Map.lookup (symbolName s) scope) = Left $ SymbolTableUpdateError $ "duplicated symbol `" ++ show (symbolName s) ++ "` in current scope"
  | otherwise = Right $ Map.insert (symbolName s) s scope : scopes

addSym :: Maybe Symbol -> Transformation ()
addSym Nothing = return ()
addSym (Just sym) = do
  env <- gets stateEnv
  case addSymbol sym env of
    Left e -> error $ show e
    Right env' -> modify $ \s -> s {stateEnv = env'}

lookupSymbol :: SymbolName -> Env -> Maybe Symbol
lookupSymbol _ [] = Nothing
lookupSymbol sn (scope : scopes) = Map.lookup sn scope <|> lookupSymbol sn scopes

lookupSym :: SymbolName -> Transformation (Maybe Symbol)
lookupSym sn = do
  env <- gets stateEnv
  return $ lookupSymbol sn env

contractSymType :: IType
contractSymType = BuiltinType "contract"

functionSymType :: IType
functionSymType = BuiltinType "function"

-- Transformation Statement Wrapper is a container of statements injected before & after target.
data TFStmtWrapper = TFStmtWrapper
  { prependStmts :: [IR.IStatement],
    appendStmts :: [IR.IStatement]
  }
  deriving (Show, Eq, Ord)

mergeTFStmtWrapper :: TFStmtWrapper -> TFStmtWrapper -> TFStmtWrapper
mergeTFStmtWrapper (TFStmtWrapper preA appA) (TFStmtWrapper preB appB) =
  TFStmtWrapper (preA ++ preB) (appA ++ appB)

wrapTFStmtWrapper :: TFStmtWrapper -> TFStmtWrapper -> TFStmtWrapper
wrapTFStmtWrapper (TFStmtWrapper preOuter appOuter) (TFStmtWrapper preInner appInner) =
  TFStmtWrapper (preOuter ++ preInner) (appInner ++ appOuter)

-----------------  IR to sCrypt  -----------------

class ToScryptTransformable ir scr where
  _toScrypt :: ir -> scr

transform2Scrypt :: ToScryptTransformable ir scr => ir -> IO scr
transform2Scrypt = return . _toScrypt