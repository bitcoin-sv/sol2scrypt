module Intermediate.Specs.Contract where

import Intermediate.Specs.Lexical
import Intermediate.Specs.Type
import Intermediate.Specs.Function
import Intermediate.Specs.Expression

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