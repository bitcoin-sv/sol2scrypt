module Intermediate.Spec
  ( module Intermediate.Specs.Lexical,
    module Intermediate.Specs.Type,
    module Intermediate.Specs.Expression,
    module Intermediate.Specs.Statement,
    module Intermediate.Specs.Function,
    module Intermediate.Specs.Contract,
    module Intermediate.Specs.Program,
    IType', IExpr'
  )
where

import Intermediate.Specs.Lexical
import Intermediate.Specs.Expression
import Intermediate.Specs.Type
import Intermediate.Specs.Statement
import Intermediate.Specs.Function
import Intermediate.Specs.Contract
import Intermediate.Specs.Program

type IType' = Maybe IType

type IExpr' = Maybe IExpr