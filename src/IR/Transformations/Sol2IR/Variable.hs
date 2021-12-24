{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Variable where

import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Identifier ()
import IR.Transformations.Sol2IR.Statement ()
import IR.Transformations.Sol2IR.Type ()
import Solidity.Spec as Sol

instance ToIRTransformable Parameter IParam' where
  _toIR (Parameter t _ (Just (Sol.Identifier pn))) = do
    t' <- _toIR t
    return $ IR.Param <$> t' <*> Just (IR.Identifier pn)
  _toIR _ = return Nothing
