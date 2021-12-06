{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Intermediate.Transformer (
  module Intermediate.Transformations.Sol2IntM.Type,
  module Intermediate.Transformations.IntM2Scr.Type,
  module Intermediate.Transformations.Sol2IntM.Expression,
  module Intermediate.Transformations.IntM2Scr.Expression
) where

import Intermediate.Spec
import Intermediate.Transformations.Sol2IntM.Type
import Intermediate.Transformations.IntM2Scr.Type
import Intermediate.Transformations.Sol2IntM.Expression
import Intermediate.Transformations.IntM2Scr.Expression

class Node a where
  nodeType :: Show a => a -> String
  nodeType t = show t

  current :: a -> a
  current a = a

instance Node IType




-- transformSol2Inter' :: Node a => String -> IO a
-- transformSol2Inter' solidityCode = do
--   tn :: TypeName <- parseIO solidityCode

--   -- let t' = transform tn
--   im <- runStateT (transform tn) TransformState

--   -- return $ fst im
--   return ()

-- class (Parseable a, Node b) => Transformable a b where
--   transform :: a -> Transformation b
-- instance Transformable TypeName IType' where
--   transform (TypeNameElementaryTypeName BoolType) = return $ Just ITypeBool
--   transform _ = return Nothing

-- class (Parseable a) => Transformable a where
--   transform :: (Node b, Monad m) => a -> m b

-- instance Transformable TypeName where
--   transform (TypeNameElementaryTypeName BoolType)  = return ITypeBool
--   transform _ = return Nothing
