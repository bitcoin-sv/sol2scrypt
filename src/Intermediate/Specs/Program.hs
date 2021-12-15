module Intermediate.Specs.Program where

import Intermediate.Specs.Contract

data IProgram = Program
  { programImports :: [IImportDirective],
    programContracts :: [IContract],
    programLibraries :: [ILibrary]
  }
  deriving (Show, Eq, Ord)

newtype IImportDirective = ImportDirective FilePath deriving (Show, Eq, Ord)