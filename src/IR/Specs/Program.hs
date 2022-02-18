module IR.Specs.Program where

import IR.Specs.Contract
import IR.Specs.Struct

data IProgram = Program
  { programImports :: [IImportDirective],
    programContracts :: [IContract],
    programLibraries :: [ILibrary],
    programStructs :: [IStruct]
  }
  deriving (Show, Eq, Ord)

newtype IImportDirective = ImportDirective FilePath deriving (Show, Eq, Ord)