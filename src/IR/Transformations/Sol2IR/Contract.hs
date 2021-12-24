{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IR.Transformations.Sol2IR.Contract where

import IR.Transformations.Base
import IR.Transformations.Sol2IR.Expression ()
import Solidity.Spec as Sol
import IR.Spec as IR
import Protolude.Functor


instance ToIRTransformable Sol.PragmaDirective IR.IEmpty where
  _toIR _ = return IR.Empty
