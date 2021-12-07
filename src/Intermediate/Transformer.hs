{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Intermediate.Transformer
  ( module Intermediate.Transformations.Base,
    -- module Intermediate.Transformations.Sol2IntM.Type,
    module Intermediate.Transformations.IntM2Scr.Type,
    module Intermediate.Transformations.Sol2IntM.Expression,
    module Intermediate.Transformations.IntM2Scr.Expression,
  )
where

import Intermediate.Spec
import Intermediate.Transformations.Base
import Intermediate.Transformations.IntM2Scr.Expression
import Intermediate.Transformations.IntM2Scr.Type
import Intermediate.Transformations.Sol2IntM.Expression
import Intermediate.Transformations.Sol2IntM.Type ()

class Node a where
  nodeType :: Show a => a -> String
  nodeType t = show t

  current :: a -> a
  current a = a

instance Node IType