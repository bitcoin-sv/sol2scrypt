{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module IR.Transformer
  ( module IR.Transformations.Base,
    -- module IR.Transformations.Sol2IntM.Type,
    module IR.Transformations.IntM2Scr.Type,
    module IR.Transformations.Sol2IntM.Expression,
    module IR.Transformations.IntM2Scr.Expression,
  )
where

import IR.Spec
import IR.Transformations.Base
import IR.Transformations.IntM2Scr.Expression
import IR.Transformations.IntM2Scr.Type
import IR.Transformations.Sol2IntM.Expression
import IR.Transformations.Sol2IntM.Type ()

class Node a where
  nodeType :: Show a => a -> String
  nodeType t = show t

  current :: a -> a
  current a = a

instance Node IType