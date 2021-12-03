module Intermediate.Spec
  ( module Intermediate.Specs.Type,
    module Intermediate.Specs.Expression,
    IType', IExpr'
  )
where

import Intermediate.Specs.Expression
import Intermediate.Specs.Type

type IType' = Maybe IType

type IExpr' = Maybe IExpr