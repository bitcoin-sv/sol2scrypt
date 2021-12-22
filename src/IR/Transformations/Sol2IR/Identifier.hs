{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IR.Transformations.Sol2IR.Identifier where

import IR.Transformations.Base
import Solidity.Spec as Sol
import IR.Spec as IR

instance ToIRTransformable Sol.Identifier IIdentifier' where
  _toIR (Sol.Identifier i) = return $ Just $ IR.Identifier i