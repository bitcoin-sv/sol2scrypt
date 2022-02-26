{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Type where

import Control.Monad.State
import qualified Data.Map.Lazy as Map
import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Expression ()
import Solidity.Spec as Sol
import Utils

-- from TypeName to IType'
instance ToIRTransformable TypeName IType' where
  _toIR (TypeNameElementaryTypeName Sol.BoolType) = return $ Just $ ElementaryType Bool
  _toIR (TypeNameElementaryTypeName (Sol.IntType _)) = return $ Just $ ElementaryType Int
  _toIR (TypeNameElementaryTypeName (Sol.UintType _)) = return $ Just $ ElementaryType Int
  _toIR (TypeNameElementaryTypeName (Sol.BytesType _)) = return $ Just $ ElementaryType Bytes
  _toIR (TypeNameElementaryTypeName Sol.ByteType) = return $ Just $ ElementaryType Bytes
  _toIR (TypeNameElementaryTypeName Sol.StringType) = return $ Just $ ElementaryType String
  _toIR (TypeNameElementaryTypeName Sol.AddressType) = return $ Just $ ElementaryType Address
  _toIR (TypeNameElementaryTypeName Sol.VarType) = return $ Just $ ElementaryType Any
  _toIR (TypeNameArrayTypeName t e) = do
    t' <- _toIR t
    sub <- _toIR e
    let arr = flip Array sub
    return $ arr <$> t'
  _toIR (TypeNameMapping kt vt) = toIRMappingType (TypeNameElementaryTypeName kt) vt []
  _toIR t = error $ "unsupported type `" ++ headWord (show t) ++ "`"

instance ToIRTransformable (TypeName' SourceRange) IType' where
  _toIR (TypeNameElementaryTypeName' (ElementaryTypeName' Sol.BoolType _) _) = return $ Just $ ElementaryType Bool
  _toIR (TypeNameElementaryTypeName' (ElementaryTypeName' (Sol.IntType _) _) _) = return $ Just $ ElementaryType Int
  _toIR (TypeNameElementaryTypeName' (ElementaryTypeName' (Sol.UintType _) _) _) = return $ Just $ ElementaryType Int
  _toIR (TypeNameElementaryTypeName' (ElementaryTypeName' (Sol.BytesType _) _) _) = return $ Just $ ElementaryType Bytes
  _toIR (TypeNameElementaryTypeName' (ElementaryTypeName' Sol.ByteType _) _) = return $ Just $ ElementaryType Bytes
  _toIR (TypeNameElementaryTypeName' (ElementaryTypeName' Sol.StringType _) _) = return $ Just $ ElementaryType String
  _toIR (TypeNameElementaryTypeName' (ElementaryTypeName' Sol.AddressType _) _) = return $ Just $ ElementaryType Address
  _toIR (TypeNameElementaryTypeName' (ElementaryTypeName' Sol.VarType _) _) = return $ Just $ ElementaryType Any
  _toIR (TypeNameArrayTypeName' t e _) = do
    t' <- _toIR t
    sub <- _toIR e
    let arr = flip Array sub
    return $ arr <$> t'
  _toIR (TypeNameMapping' kt vt _) = toIRMappingType' (TypeNameElementaryTypeName' kt $ ann kt) vt []
  _toIR t = error $ "unsupported type `" ++ headWord (show t) ++ "`"

-- transpile Sol mapping type to a flattened IR mapping type
toIRMappingType :: TypeName -> TypeName -> [IType'] -> Transformation IType'
toIRMappingType kt (TypeNameMapping vkt vvt) flattendKeyTypes = do
  kt' <- _toIR kt
  toIRMappingType (TypeNameElementaryTypeName vkt) vvt $ flattendKeyTypes ++ [kt']
toIRMappingType kt vt flattendKeyTypes = do
  kt' <- _toIR kt
  vt' <- _toIR vt
  kt'' <-
    if null flattendKeyTypes
      then return kt' -- for non nested mapping type, e.x. `a[b]`
      else do
        -- for nested mapping type, e.x. `a[b][c]`
        mapKeySTs <- gets stateMapKeyStructs
        case sequence $ flattendKeyTypes ++ [kt'] of
          Just kts -> do
            case Map.lookup kts mapKeySTs of
              Just (IR.Struct sn _) ->
                return $ Just $ UserDefinedType sn
              _ -> do
                let sn = "MapKeyST" ++ show (length mapKeySTs)
                -- use a new struct for flattened map key types
                modify $ \s ->
                  s
                    { stateMapKeyStructs =
                        Map.insert
                          kts
                          ( IR.Struct sn $
                              zipWith (\ft i -> Param ft $ IR.Identifier $ "key" ++ show i) kts [0 ..]
                          )
                          mapKeySTs
                    }
                return $ Just $ UserDefinedType sn
          _ -> return Nothing
  return $ Mapping <$> kt'' <*> vt'

toIRMappingType' :: TypeName' SourceRange -> TypeName' SourceRange -> [IType'] -> Transformation IType'
toIRMappingType' kt (TypeNameMapping' vkt vvt a) flattendKeyTypes = do
  kt' <- _toIR kt
  toIRMappingType' (TypeNameElementaryTypeName' vkt $ mergeRange (ann kt) a) vvt $ flattendKeyTypes ++ [kt']
toIRMappingType' kt vt flattendKeyTypes = do
  kt' <- _toIR kt
  vt' <- _toIR vt
  kt'' <-
    if null flattendKeyTypes
      then return kt' -- for non nested mapping type, e.x. `a[b]`
      else do
        -- for nested mapping type, e.x. `a[b][c]`
        mapKeySTs <- gets stateMapKeyStructs
        case sequence $ flattendKeyTypes ++ [kt'] of
          Just kts -> do
            case Map.lookup kts mapKeySTs of
              Just (IR.Struct sn _) ->
                return $ Just $ UserDefinedType sn
              _ -> do
                let sn = "MapKeyST" ++ show (length mapKeySTs)
                -- use a new struct for flattened map key types
                modify $ \s ->
                  s
                    { stateMapKeyStructs =
                        Map.insert
                          kts
                          ( IR.Struct sn $
                              zipWith (\ft i -> Param ft $ IR.Identifier $ "key" ++ show i) kts [0 ..]
                          )
                          mapKeySTs
                    }
                return $ Just $ UserDefinedType sn
          _ -> return Nothing
  return $ Mapping <$> kt'' <*> vt'