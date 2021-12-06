module Intermediate.Transformations.IntM2Scr.Type where

import Intermediate.Spec as IntM
import Scrypt.Spec as Scr

-----------------  Intermediate to sCrypt  -----------------

transformIntermediateType :: IType -> Type
transformIntermediateType ITypeBool = Scr.Bool
transformIntermediateType ITypeInt = Scr.Int
transformIntermediateType ITypeBytes = Scr.Bytes
transformIntermediateType ITypeAddress = Scr.SubBytes Scr.PubKey
transformIntermediateType t = error $ "Type `" ++ show t ++ "` not implemented in scrypt"
