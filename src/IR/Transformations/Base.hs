{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Transformations.Base where

import Control.Applicative hiding (Const)
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Control.Monad.State
import Solidity.Parser
import IR.Spec as IR
import Text.Parsec hiding (try, (<|>))


parseIO :: Parseable a => String -> IO a
parseIO solidityCode = either (fail . (parseError ++) . show) return $ parse parser "" solidityCode
  where
    parseError = "Error during parsing of <" ++ solidityCode ++ ">\n"

-----------------  Solidity to IR -----------------

newtype TransformState = TransformState { stateEnv :: Env} deriving (Show, Eq, Ord)

type Transformation a = StateT TransformState IO a

class Parseable sol => ToIRTransformable sol ir where
  _toIR :: sol -> Transformation ir

transform2IR :: ToIRTransformable sol ir => TransformState -> sol -> IO ir
transform2IR ts sol = fst <$> runStateT (_toIR sol) ts

-- symbols & enviroment

type SymbolName = IIdentifier

data Symbol = Symbol {
    symbolName :: SymbolName,
    symbolType :: IType,
    symbolStateVar :: Bool
  } deriving (Show, Eq, Ord)

type SymbolTable = Map.Map SymbolName Symbol

data GState = GState {
    symbolTable :: [SymbolTable],
    returned :: Returned
  } deriving (Show, Eq, Ord)

type Env = GState

addScope :: Env -> Env
addScope gstate = GState (Map.empty : symbolTable gstate) (returned gstate)

dropScope :: Env -> Env
dropScope gstate = GState (tail $ symbolTable gstate) (returned gstate) 

enterScope :: Transformation ()
enterScope = do
  env <- gets stateEnv
  modify $ \s -> s {stateEnv = addScope env}
  return ()

_setReturned :: Env -> Bool -> Env
_setReturned gstate r = GState (symbolTable gstate) (Returned r)

setReturned :: Transformation ()
setReturned = do
  env <- gets stateEnv
  modify $ \s -> s {stateEnv = _setReturned env True}
  return ()

clearReturned :: Transformation ()
clearReturned = do
  env <- gets stateEnv
  modify $ \s -> s {stateEnv = _setReturned env False}
  return ()


isReturned :: Transformation Bool
isReturned = do
  env <- gets stateEnv
  return (returned env == Returned True)




leaveScope :: Transformation ()
leaveScope = do
  env <- gets stateEnv
  modify $ \s -> s {stateEnv = dropScope env}
  return ()

newtype SymbolTableUpdateError = SymbolTableUpdateError String deriving (Show, Eq, Ord)

addSymbol :: Symbol -> Env -> Either SymbolTableUpdateError Env
addSymbol s (GState [] r) = addSymbol s $ GState [Map.empty] r
addSymbol s (GState (scope : scopes) r) 
  | isJust (Map.lookup (symbolName s) scope) = Left $ SymbolTableUpdateError $ "duplicated symbol `" ++ show (symbolName s) ++ "` in current scope"
  | otherwise =  Right $ GState (Map.insert (symbolName s) s scope : scopes) r

addSym :: Maybe Symbol -> Transformation ()
addSym Nothing = return ()
addSym (Just sym) = do
  env <- gets stateEnv
  case addSymbol sym env of
    Left e -> error $ show e
    Right env' -> modify $ \s -> s {stateEnv = env'}

lookupSymbol :: SymbolName -> Env -> Maybe Symbol
lookupSymbol _ (GState [] _)  = Nothing
lookupSymbol sn (GState (scope: scopes) r) = Map.lookup sn scope <|> lookupSymbol sn (GState scopes r)

lookupSym :: SymbolName -> Transformation (Maybe Symbol)
lookupSym sn = do
  env <- gets stateEnv
  return $ lookupSymbol sn env

contractSymType :: IType
contractSymType = BuiltinType "contract"

functionSymType :: IType
functionSymType = BuiltinType "function"

-- returned

newtype Returned = Returned Bool deriving (Show, Eq, Ord)


-----------------  IR to sCrypt  -----------------

class ToScryptTransformable ir scr where
  _toScrypt :: ir -> scr

transform2Scrypt :: ToScryptTransformable ir scr => ir -> IO scr
transform2Scrypt = return . _toScrypt