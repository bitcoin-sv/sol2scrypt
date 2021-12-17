{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IR.Transformer
  ( module IR.Transformations.Base,
    -- module IR.Transformations.Sol2IR.Type,
    module IR.Transformations.IR2Scr.Type,
    module IR.Transformations.Sol2IR.Expression,
    module IR.Transformations.IR2Scr.Expression,
  )
where

import IR.Spec
import IR.Transformations.Base
import IR.Transformations.IR2Scr.Expression
import IR.Transformations.IR2Scr.Type
import IR.Transformations.Sol2IR.Expression
import IR.Transformations.Sol2IR.Type ()

class Node a where
  nodeType :: Show a => a -> String
  nodeType t = show t

  current :: a -> a
  current a = a

instance Node IType