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
import Data.Maybe
import Utils

-- from TypeName to IType'
instance ToIRTransformable (TypeName SourceRange) IType' where
  _toIR (TypeNameElementaryTypeName (ElementaryTypeName Sol.BoolType _) _) = return $ Just $ ElementaryType Bool
  _toIR (TypeNameElementaryTypeName (ElementaryTypeName (Sol.IntType _) _) _) = return $ Just $ ElementaryType Int
  _toIR (TypeNameElementaryTypeName (ElementaryTypeName (Sol.UintType _) _) _) = return $ Just $ ElementaryType Int
  _toIR (TypeNameElementaryTypeName (ElementaryTypeName (Sol.BytesType _) _) _) = return $ Just $ ElementaryType Bytes
  _toIR (TypeNameElementaryTypeName (ElementaryTypeName Sol.ByteType _) _) = return $ Just $ ElementaryType Bytes
  _toIR (TypeNameElementaryTypeName (ElementaryTypeName Sol.StringType _) _) = return $ Just $ ElementaryType String
  _toIR (TypeNameElementaryTypeName (ElementaryTypeName Sol.AddressType _) _) = return $ Just $ ElementaryType Address
  _toIR (TypeNameElementaryTypeName (ElementaryTypeName Sol.VarType _) _) = return $ Just $ ElementaryType Any
  _toIR (TypeNameArrayTypeName t e a) = do
    t' <- _toIR t
    sub <- case e of
             Just e' -> do
               sub' <- _toIR e'
               return $ Just sub'
             _ -> reportError "array length should be explicitly specified" a >> return Nothing
    return $ createArray <$> t' <*> sub

  _toIR (TypeNameMapping kt vt _) = toIRMappingType (TypeNameElementaryTypeName kt $ ann kt) vt []
  _toIR t@(TypeNameUserDefinedTypeName (UserDefinedTypeName [Sol.Identifier i _]) _) = do
    st' <- lookupStruct i
    if isJust st' 
      then return $ Just$ UserDefinedType i
      else do
        reportError ("unsupported type `" ++ headWord (show t) ++ "`") (ann t) >> return Nothing
  _toIR t = reportError ("unsupported type `" ++ headWord (show t) ++ "`") (ann t) >> return Nothing

-- transpile Sol mapping type to a flattened IR mapping type
toIRMappingType :: TypeName SourceRange -> TypeName SourceRange -> [IType'] -> Transformation IType'
toIRMappingType kt (TypeNameMapping vkt vvt a) flattendKeyTypes = do
  kt' <- _toIR kt
  toIRMappingType (TypeNameElementaryTypeName vkt $ mergeRange (ann kt) a) vvt $ flattendKeyTypes ++ [kt']
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
                              zipWith (\ft i -> Param ft $ IR.Identifier $ "key" ++ show (i :: Integer)) kts [0 ..]
                          )
                          mapKeySTs
                    }
                return $ Just $ UserDefinedType sn
          _ -> return Nothing
  return $ Mapping <$> kt'' <*> vt'


createArray :: IType -> Int  -> IType
createArray (Array t n') n = Array (createArray t n) n'
createArray t n = Array t n