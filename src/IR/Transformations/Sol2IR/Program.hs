{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module IR.Transformations.Sol2IR.Program where

import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Expression ()
import IR.Transformations.Sol2IR.Contract ()
import Solidity.Spec as Sol
import Data.Maybe (catMaybes)
import System.FilePath (replaceExtensions)

import Utils

-- from SolidityCode to IProgram'


instance ToIRTransformable Sol.SourceUnit1 IContract' where
  _toIR (Sol.SourceUnit1_ContractDefinition contractDef ) = _toIR contractDef

instance ToIRTransformable Sol.ImportDirective IImportDirective' where
  _toIR (Sol.ImportDirective _ (StringLiteral path)) = return $ Just $ IR.ImportDirective $ replaceExtensions path "scrypt"

instance ToIRTransformable Sol.SourceUnit1 IImportDirective' where
  _toIR (Sol.SourceUnit1_ImportDirective ip ) = _toIR ip

instance ToIRTransformable Sol.SolidityCode IProgram' where
  _toIR (Sol.SolidityCode (SourceUnit [])) = return $ Just $ IR.Program [] [] []
  _toIR (Sol.SolidityCode (SourceUnit sourceUnits)) = do
    let contracts =
          filter
            ( \d -> case d of
                SourceUnit1_ContractDefinition _ -> True
                _ -> False
            )
            sourceUnits
    contracts' <- mapM _toIR contracts

    let imports =
          filter
            ( \d -> case d of
                SourceUnit1_ImportDirective _ -> True
                _ -> False
            )
            sourceUnits
    imports' <- mapM _toIR imports

    return $ Just $ IR.Program (catMaybes imports') (catMaybes contracts') []
