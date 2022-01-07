{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IR.Transformations.Sol2IR.Program where

import IR.Transformations.Base
import Solidity.Spec as Sol
import IR.Spec as IR
import Utils
import IR.Transformations.Sol2IR.Expression ()

-- from SolidityCode to IProgram'
instance ToIRTransformable Sol.SolidityCode  IProgram' where
  _toIR (Sol.SolidityCode _ ) = return $ Just $ IR.Program [] [] []
