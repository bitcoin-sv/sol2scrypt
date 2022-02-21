module IR.Spec
  ( module IR.Specs.Lexical,
    module IR.Specs.Type,
    module IR.Specs.Struct,
    module IR.Specs.Expression,
    module IR.Specs.Variable,
    module IR.Specs.Statement,
    module IR.Specs.Function,
    module IR.Specs.Contract,
    module IR.Specs.Empty,
    module IR.Specs.Program,
    IIdentifier',
    IElementaryTypeName',
    IType',
    IStruct',
    IExpression',
    IStatement',
    IParam',
    IParamList',
    IVisibility',
    IBlock',
    IFunction',
    IContractBodyElement',
    IStateVariable',
    IContract',
    IConstructor',
    IProgram',
    IImportDirective',
  )
where

import IR.Specs.Contract
import IR.Specs.Empty
import IR.Specs.Expression
import IR.Specs.Function
import IR.Specs.Lexical
import IR.Specs.Program
import IR.Specs.Statement
import IR.Specs.Struct
import IR.Specs.Type
import IR.Specs.Variable

type IIdentifier' = Maybe IIdentifier

type IElementaryTypeName' = Maybe IElementaryTypeName

type IType' = Maybe IType

type IExpression' = Maybe IExpression

type IStatement' = Maybe IStatement

type IVisibility' = Maybe IVisibility

type IBlock' = Maybe IBlock

type IParam' = Maybe IParam

type IParamList' = Maybe IParamList

type IFunction' = Maybe IFunction

type IConstructor' = Maybe IConstructor

type IStateVariable' = Maybe IStateVariable

type IContractBodyElement' = Maybe IContractBodyElement

type IContract' = Maybe IContract

type IProgram' = Maybe IProgram

type IImportDirective' = Maybe IImportDirective

type IStruct' = Maybe IStruct