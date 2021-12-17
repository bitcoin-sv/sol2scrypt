module IR.Specs.Contract where

import IR.Specs.Lexical
import IR.Specs.Type
import IR.Specs.Function
import IR.Specs.Expression

data IContract = Contract 
  {
    contractName :: IIdentifier,
    contractBody :: [IContractBodyElement]
  }
  deriving (Show, Eq, Ord)

data ILibrary = Library
  {
    libraryName :: IIdentifier,
    libraryBody :: [IContractBodyElement]
  }
  deriving (Show, Eq, Ord)

data IStateVariable = StateVariable
  {
    stateVarName :: IIdentifier,
    stateVarType :: IType,
    stateInitialValue :: Maybe IExpr
  }
  deriving (Show, Eq, Ord)

data IContractBodyElement
  = StateVariableDeclaration IStateVariable
  | ConstructorDefinition IConstructor
  | FunctionDefinition IFunction
  deriving (Show, Eq, Ord)