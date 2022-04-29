{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Transformations.Base where

import Control.Applicative hiding (Const)
import Control.Monad.State
import Control.Monad.Writer hiding (Any)
import Data.Either
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Set
import IR.Spec as IR
import Solidity.Parser
import Solidity.Spec
import Text.Parsec hiding (try, (<|>))

parseIO :: Parseable a => String -> SourceName -> IO a
parseIO solidityCode file = either (fail . (parseError ++) . show) return $ parse parser file solidityCode
  where
    parseError = "Error during parsing of <" ++ solidityCode ++ ">\n"

-----------------  Solidity to IR -----------------
data TransformState = TransformState
  { stateEnv :: Env,
    -- TFStmtWrapper for current statement
    stateWrapperForStmt :: Maybe TFStmtWrapper,
    -- counter for mapping-access exprs in a function, key is the expression name, value is its occurrences count
    stateInFuncMappingCounter :: MappingExprCounter,
    -- have returned previously in current block, the first element represents the innermost for nested loops
    stateReturnedInBlock :: [Bool],
    -- structs used as key type for nested maps
    stateMapKeyStructs :: Map.Map [IType] IStruct,
    stateStructs :: [IStruct],
    -- loop count in function, also used for loop id
    stateInFuncLoopCount :: Integer,
    -- nested loop ids, the first element is the innermost loop id
    stateNestedLoops :: [Integer],
    stateInLibrary :: Bool,
    -- loop ids which have `break` statement(s) inside in a function
    stateInFuncBrokeLoops :: Set Integer,
    -- loop ids which have `continue` statement(s) inside in a function
    stateInFuncContinuedLoops :: Set Integer,
    -- have `break` /`continue` statement in loop's nested blocks, the first element represents the innermost block's flag
    stateBCInBlock :: [BreakContinueInBlock],
    -- whether contract's balance got changed in function
    stateBalanceChanged :: Bool,
    -- need generate a contract property (`initBalance`) to store the initial value of contract balance
    stateNeedInitBalace :: Bool
  }
  deriving (Show, Eq, Ord)

type BreakFlag = IR.IIdentifier'

type ContinueFlag = IR.IIdentifier'

-- the `break` & `continue` flags in a block
type BreakContinueInBlock = (BreakFlag, ContinueFlag)

data LogLevel = ErrorLevel | WarnningLevel deriving (Eq, Ord)

instance Show LogLevel where
  show ErrorLevel = "Error"
  show WarnningLevel = "Warnning"

data Log = Log
  { logLevel :: LogLevel,
    logMessage :: String,
    logSrc :: SourceRange
  }
  deriving (Show, Eq, Ord)

type Logs = [Log]

type Transformation a = StateT TransformState (Writer Logs) a

reportError :: String -> SourceRange -> Transformation ()
reportError err src = tell [Log ErrorLevel err src]

class Parseable sol => ToIRTransformable sol ir where
  _toIR :: sol -> Transformation ir

transform2IR :: ToIRTransformable sol ir => TransformState -> sol -> IO (ir, Logs)
transform2IR ts sol = return (ir, logs)
  where
    ((ir, _), logs) = runWriter $ runStateT (_toIR sol) ts

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
    exprCnt :: Int,
    -- whether the entry been updated, i.e. used as LHS expr.
    entryUpdated :: Bool,
    index :: Int
  }
  deriving (Show, Eq, Ord)

-- symbols & enviroment
type SymbolName = IIdentifier

data Symbol = Symbol
  { symbolName :: SymbolName,
    symbolType :: IType,
    symbolStateVar :: Bool,
    symbolIsConstant :: Bool,
    symbolStatic :: Bool
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

addSym :: Maybe Symbol -> Transformation (Either SymbolTableUpdateError Env)
addSym Nothing = return $ Left $ SymbolTableUpdateError "addSym Nothing"
addSym (Just sym) = do
  env <- gets stateEnv
  let r = addSymbol sym env
  when (isRight r) $ do
    modify $ \s -> s {stateEnv = fromRight env r}
  return r

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

mergeTFStmtWrapper' :: Maybe TFStmtWrapper -> Maybe TFStmtWrapper -> Maybe TFStmtWrapper
mergeTFStmtWrapper' (Just (TFStmtWrapper preA appA)) (Just (TFStmtWrapper preB appB)) =
  Just $ TFStmtWrapper (preA ++ preB) (appA ++ appB)
mergeTFStmtWrapper' _ _ = Nothing

wrapTFStmtWrapper :: TFStmtWrapper -> TFStmtWrapper -> TFStmtWrapper
wrapTFStmtWrapper (TFStmtWrapper preOuter appOuter) (TFStmtWrapper preInner appInner) =
  TFStmtWrapper (preOuter ++ preInner) (appInner ++ appOuter)

sourceFile :: Log -> FilePath
sourceFile (Log _ _ (SourceRange start _)) = sourceName start

serializeSourceRange :: SourceRange -> String
serializeSourceRange (SourceRange start end) = sourceName start ++ ":" ++ showPos start ++ ":" ++ showPos end
  where
    showPos p = show (sourceLine p) ++ ":" ++ show (sourceColumn p)

lookupStruct :: String -> Transformation (Maybe IStruct)
lookupStruct sn = do
  ss <- gets stateStructs
  return $ find (\s -> structName s == sn) ss

enterLibrary :: Bool -> Transformation ()
enterLibrary isLibrary = modify $ \s ->
  s
    { stateInLibrary = isLibrary
    }

isInLibrary :: Transformation Bool
isInLibrary = gets stateInLibrary

-----------------  IR to sCrypt  -----------------

class ToScryptTransformable ir scr where
  _toScrypt :: ir -> scr

transform2Scrypt :: ToScryptTransformable ir scr => ir -> IO scr
transform2Scrypt = return . _toScrypt