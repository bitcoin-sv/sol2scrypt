{-# LANGUAGE ScopedTypeVariables #-}
module Intermediate.Transformations.Type where

import Intermediate.Transformations.Base
import Solidity.Spec as Sol
import Intermediate.Spec as IntM
import Scrypt.Spec as Scr
import Control.Monad.State

-----------------  Solidity to Intermediate -----------------

transformSolTypeName :: String -> IO IType'
transformSolTypeName solidityCode = do
  tn :: TypeName <- parseIO solidityCode
  fst <$> runStateT (solTypeName2Intermediate tn) TransformState

solTypeName2Intermediate :: TypeName -> Transformation IType'
solTypeName2Intermediate (TypeNameElementaryTypeName BoolType) = return $ Just ITypeBool
solTypeName2Intermediate (TypeNameElementaryTypeName (IntType _)) = return $ Just ITypeInt
solTypeName2Intermediate (TypeNameElementaryTypeName (UintType _)) = return $ Just ITypeInt
solTypeName2Intermediate (TypeNameElementaryTypeName (BytesType _)) = return $ Just ITypeBytes
solTypeName2Intermediate (TypeNameElementaryTypeName ByteType) = return $ Just ITypeBytes
solTypeName2Intermediate (TypeNameElementaryTypeName StringType) = return $ Just ITypeString
solTypeName2Intermediate _ = return Nothing


-----------------  Intermediate to sCrypt  -----------------

transformIntermediateType :: IType -> Type
transformIntermediateType ITypeBool = Scr.Bool
transformIntermediateType ITypeInt = Scr.Int
transformIntermediateType ITypeBytes = Scr.Bytes
transformIntermediateType ITypeAddress = Scr.SubBytes Scr.PubKey
transformIntermediateType t = error $ "Type `" ++ show t ++ "` not implemented in scrypt"
