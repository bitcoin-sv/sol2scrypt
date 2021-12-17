module IR.Specs.Program where

import IR.Specs.Contract

data IProgram = Program
  { programImports :: [IImportDirective],
    programContracts :: [IContract],
    programLibraries :: [ILibrary]
  }
  deriving (Show, Eq, Ord)

newtype IImportDirective = ImportDirective FilePath deriving (Show, Eq, Ord)