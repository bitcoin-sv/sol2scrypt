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

newtype TransformState = TransformState { stateEnv :: Env } deriving (Show, Eq, Ord)

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

type Env = [SymbolTable]

addScope :: Env -> Env
addScope scopes = Map.empty : scopes

dropScope :: Env -> Env
dropScope = tail

enterScope :: Transformation ()
enterScope = do
  env <- gets stateEnv
  modify $ \s -> s {stateEnv = addScope env}
  return ()

leaveScope :: Transformation ()
leaveScope = do
  env <- gets stateEnv
  modify $ \s -> s {stateEnv = dropScope env}
  return ()

newtype SymbolTableUpdateError = SymbolTableUpdateError String deriving (Show, Eq, Ord)

addSymbol :: Symbol -> Env -> Either SymbolTableUpdateError Env
addSymbol s [] = addSymbol s [Map.empty]
addSymbol s (scope : scopes)
  | isJust (Map.lookup (symbolName s) scope) = Left $ SymbolTableUpdateError $ "duplicated symbol `" ++ show (symbolName s) ++ "` in current scope"
  | otherwise =  Right $ Map.insert (symbolName s) s scope : scopes

addSym :: Maybe Symbol -> Transformation ()
addSym Nothing = return ()
addSym (Just sym) = do
  env <- gets stateEnv
  case addSymbol sym env of
    Left e -> error $ show e
    Right env' -> modify $ \s -> s {stateEnv = env'}

lookupSymbol :: SymbolName -> Env -> Maybe Symbol
lookupSymbol _ [] = Nothing
lookupSymbol sn (scope: scopes) = Map.lookup sn scope <|> lookupSymbol sn scopes

lookupSym :: SymbolName -> Transformation (Maybe Symbol)
lookupSym sn = do
  env <- gets stateEnv
  return $ lookupSymbol sn env

contractSymType :: IType
contractSymType = BuiltinType "contract"

functionSymType :: IType
functionSymType = BuiltinType "function"

-----------------  IR to sCrypt  -----------------

class ToScryptTransformable ir scr where
  _toScrypt :: ir -> scr

transform2Scrypt :: ToScryptTransformable ir scr => ir -> IO scr
transform2Scrypt = return . _toScrypt