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

instance ToScryptTransformable IR.IContract (Scr.Contract Ann) where
  _toScrypt (IR.Contract cn bodyElems) = Scr.Contract (_toScrypt cn) [] props [] Nothing {-- constructor --} functions False nil
    where
      props =
        map _toScrypt $
          filter
            ( \case
                IR.StateVariableDeclaration _ -> True
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
  _toScrypt (IR.StateVariableDeclaration stateVar) = _toScrypt stateVar

instance ToScryptTransformable IR.IContractBodyElement (Scr.Function Ann) where
  _toScrypt (IR.FunctionDefinition function) = _toScrypt function

instance ToScryptTransformable IR.IContractBodyElement' Scr.Empty where
  _toScrypt (Just IR.EventDefinition) = Scr.Empty