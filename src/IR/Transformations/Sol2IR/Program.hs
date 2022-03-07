{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Program where

import Control.Monad.State
import qualified Data.Map.Lazy as Map
import Data.Maybe (catMaybes)
import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Contract ()
import IR.Transformations.Sol2IR.Expression ()
import Solidity.Spec as Sol
import System.FilePath (replaceExtensions)

-- from SolidityCode to IProgram'

instance ToIRTransformable (Sol.SolidityCode SourceRange) IProgram' where
  _toIR (Sol.SolidityCode (SourceUnit sourceUnits)) = do
    let contracts =
          filter
            ( \case
                SourceUnit1_ContractDefinition _ -> True
                _ -> False
            )
            sourceUnits
    contracts' <- mapM _toIR contracts

    let imports_ =
          filter
            ( \case
                SourceUnit1_ImportDirective _ -> True
                _ -> False
            )
            sourceUnits
    -- SourceUnit1_PragmaDirective is ignored here, because we transpile it to nothing
    imports' <- mapM _toIR imports_

    structs <- gets stateMapKeyStructs
    structs' <- gets stateStructs
    return $ Just $ IR.Program (catMaybes imports') (catMaybes contracts') [] $ reverse (Map.elems structs) ++ structs'

instance ToIRTransformable (Sol.SourceUnit1 SourceRange) IContract' where
  _toIR (Sol.SourceUnit1_ContractDefinition contractDef) = _toIR contractDef

instance ToIRTransformable (Sol.SourceUnit1 SourceRange) IImportDirective' where
  _toIR (Sol.SourceUnit1_ImportDirective ip) = _toIR ip

instance ToIRTransformable (Sol.ImportDirective SourceRange) IImportDirective' where
  _toIR (Sol.ImportDirective _ (Sol.StringLiteral path _) _) = return $ Just $ IR.ImportDirective $ replaceExtensions path "scrypt"
