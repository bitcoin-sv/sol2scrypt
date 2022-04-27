{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.IR2Scr.Contract where

import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.IR2Scr.Function ()
import IR.Transformations.IR2Scr.Identifier ()
import IR.Transformations.IR2Scr.Type ()
import IR.Transformations.IR2Scr.Variable ()
import Scrypt.Spec as Scr
import Utils

instance ToScryptTransformable IContract' (Maybe (Scr.Contract Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable ILibrary' (Maybe (Scr.Contract Ann)) where
  _toScrypt = (<$>) _toScrypt

maybeHead :: [Maybe (Constructor Ann)] -> Maybe (Constructor Ann)
maybeHead [] = Nothing
maybeHead (x:_) = x

instance ToScryptTransformable IR.IContract (Scr.Contract Ann) where
  _toScrypt (IR.Contract cn bodyElems) = Scr.Contract (_toScrypt cn) [] props staticProps ctor functions False nil
    where
      props =
        map _toScrypt $
          filter
            ( \case
                IR.PropertyDefinition (IR.Property _ _ _ _ _ (IsStatic False) _) -> True
                _ -> False
            )
            bodyElems
      staticProps =
        map _toScrypt $
          filter
            ( \case
                IR.PropertyDefinition (IR.Property _ _ _ _ _ (IsStatic True) _) -> True
                _ -> False
            )
            bodyElems
      ctor =
        maybeHead $ map _toScrypt $
          filter
            ( \case
                IR.ConstructorDefinition _ -> True
                _ -> False
            )
            bodyElems
      functions =
        map _toScrypt $
          filter
            ( \case
                IR.FunctionDefinition _ -> True
                _ -> False
            )
            bodyElems


instance ToScryptTransformable IR.ILibrary (Scr.Contract Ann) where
  _toScrypt (IR.Library cn bodyElems) = Scr.Contract (_toScrypt cn) [] [] staticProps Nothing functions True nil
    where
      staticProps =
        map _toScrypt $
          filter
            ( \case
                IR.PropertyDefinition (IR.Property _ _ _ _ _ (IsStatic True) _) -> True
                _ -> False
            )
            bodyElems
      functions =
        map _toScrypt $
          filter
            ( \case
                IR.FunctionDefinition _ -> True
                _ -> False
            )
            bodyElems

instance ToScryptTransformable IContractBodyElement' (Maybe (Scr.Param Ann)) where
  _toScrypt = (<$>) _toScrypt

instance ToScryptTransformable IR.IContractBodyElement (Scr.Param Ann) where
  _toScrypt (IR.PropertyDefinition stateVar) = _toScrypt stateVar

instance ToScryptTransformable IContractBodyElement' (Maybe (Scr.Static Ann)) where
  _toScrypt = (<$>) _toScrypt
instance ToScryptTransformable IR.IContractBodyElement  (Scr.Static Ann) where
  _toScrypt (IR.PropertyDefinition prop) = _toScrypt prop

instance ToScryptTransformable IR.IContractBodyElement (Scr.Function Ann) where
  _toScrypt (IR.FunctionDefinition function) = _toScrypt function


instance ToScryptTransformable IR.IContractBodyElement (Maybe (Scr.Constructor Ann)) where
  _toScrypt (IR.ConstructorDefinition (IR.Constructor (IR.ParamList pl) (IR.Block stmts) )) = Just $ Scr.Constructor (map _toScrypt pl) (Scr.CtorBody (map _toScrypt stmts) nil) nil
