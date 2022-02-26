-- Copyright 2017 Gordon J. Pace and Joshua Ellul
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

-- Bug in Sublime Text syntax highlighting for Haskel ("()"...

module Solidity.Spec (
  SolidityCode (..),
    SourceUnit (..),
      SourceUnit1 (..),
    PragmaDirective (..),
      VersionComparator (..), Version (..),
    ImportDirective (..),
      ImportDirective1(..), Import (..),
    ContractDefinition (..),
      ContractPart (..),
      StateVariableDeclaration (..),
      InheritanceSpecifier (..),
      ModifierInvocation (..),
      FunctionDefinitionTag (..),
        VariableDeclaration (..),
        Statement (..),
        TupleExpression (..), ExpressionList (..), Expression (..), PrimaryExpression (..), NameValueList (..),
        NumberLiteral (..), NumberUnit (..), HexLiteral (..), StringLiteral (..), BooleanLiteral (..),
        InlineAssemblyBlock (..), AssemblyItem (..), FunctionalAssemblyExpression (..),
        Block (..),

  IdentifierList (..), Identifier (..),
  IndexedParameterList (..), IndexedParameter (..),
  UntypedParameterList (..), ParameterList (..), Parameter (..),
  TypeNameList (..), TypeName (..), UserDefinedTypeName (..), ElementaryTypeName (..),
  StateMutability (..), StorageLocation (..),

  FunctionName, VariableName, ContractName, ModifierName,

  -- new versions
  SourceRange (..), Identifier' (..), TypeName' (..), ElementaryTypeName' (..), TypeNameList' (..), StateMutability' (..), UserDefinedTypeName' (..),
  Expression' (..), TupleExpression' (..), ExpressionList' (..),  PrimaryExpression' (..), NameValueList' (..),
  NumberLiteral' (..), HexLiteral' (..), StringLiteral' (..), BooleanLiteral' (..),
  SolidityCode' (..), SourceUnit' (..), ImportDirective' (..), ImportDirective1' (..),
  ContractDefinition' (..), ContractPart' (..), StateVariableDeclaration' (..), InheritanceSpecifier' (..),
  ModifierInvocation' (..), FunctionDefinitionTag' (..), IndexedParameterList' (..), IndexedParameter' (..),
  UntypedParameterList' (..), ParameterList' (..), Parameter' (..), VariableDeclaration' (..),
  StorageLocation' (..), IdentifierList' (..), Block' (..),
  Statement' (..), Import' (..), PragmaDirective' (..), SourceUnit1' (..),


  Annotated (..),
  untypeParameterList, typeParameterList, addMemoryLocationToParametersList, mergeRange
) where

import Data.Maybe
import Text.Parsec.Pos

data SourceRange = SourceRange
  { srcStart :: SourcePos,
    srcEnd :: SourcePos
  }
  deriving (Show, Eq, Ord)

mergeRange :: SourceRange -> SourceRange -> SourceRange
mergeRange start end = start {srcEnd = srcEnd end}

class Annotated a b where
  -- extract annotation
  ann :: a b -> b

type FunctionName = Identifier
type VariableName = Identifier
type ContractName = Identifier
type ModifierName = Identifier

-- data String' a = String' String a deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
newtype SolidityCode = SolidityCode SourceUnit deriving (Eq, Ord, Show)
newtype SolidityCode' a = SolidityCode' (SourceUnit' a) deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- SourceUnit = (PragmaDirective | ImportDirective | ContractDefinition)*

newtype SourceUnit = SourceUnit [SourceUnit1] deriving (Show, Eq, Ord)
newtype SourceUnit' a = SourceUnit' [SourceUnit1' a] deriving (Show, Eq, Ord)

data SourceUnit1
  = SourceUnit1_PragmaDirective PragmaDirective 
  | SourceUnit1_ImportDirective ImportDirective
  | SourceUnit1_ContractDefinition ContractDefinition
  deriving (Show, Eq, Ord)

data SourceUnit1' a
  = SourceUnit1_PragmaDirective' (PragmaDirective' a)
  | SourceUnit1_ImportDirective' (ImportDirective' a)
  | SourceUnit1_ContractDefinition' (ContractDefinition' a)
  deriving (Show, Eq, Ord)

instance Annotated SourceUnit1' a where
  ann (SourceUnit1_PragmaDirective' p) = ann p
  ann (SourceUnit1_ImportDirective' i) = ann i
  ann (SourceUnit1_ContractDefinition' c) = ann c

-------------------------------------------------------------------------------
-- VersionComparator = '^' | '>' | '<' | '<=' | '>='

data VersionComparator = Less | More | Equal | LessOrEqual | MoreOrEqual deriving (Show, Eq, Ord)

-- Version = VersionComparator ([0-9]+\.)+

data Version = Version VersionComparator [Int] deriving (Show, Eq, Ord)

-- PragmaDirective = 'pragma' ('solidity' | 'experimental' ) 
--                       ( (VersionComparator ' ' Version) ('||' (VersionComparator ' ' Version))*
--                        | (VersionComparator ' ' Version) (' ' (VersionComparator ' ' Version))*) ';'

data PragmaDirective = SolidityPragmaConjunction [Version] 
                     | SolidityPragmaDisjunction [Version] 
                     | ExperimentalPragma String deriving (Show, Eq, Ord)

data PragmaDirective' a = SolidityPragmaConjunction' [Version] a
                     | SolidityPragmaDisjunction' [Version] a
                     | ExperimentalPragma' String a deriving (Show, Eq, Ord)

instance Annotated PragmaDirective' a where
  ann (SolidityPragmaConjunction' _ a) = a
  ann (SolidityPragmaDisjunction' _ a) = a
  ann (ExperimentalPragma' _ a) = a
-------------------------------------------------------------------------------
-- ImportDirective = 'import' StringLiteral ('as' Identifier)? ';'
--         | 'import' ('*' | Identifier) ('as' Identifier)? 'from' StringLiteral ';'
--         | 'import' '{' Identifier ('as' Identifier)? ( ',' Identifier ('as' Identifier)? )* '}' 'from' StringLiteral ';'

data ImportDirective =
  ImportDirective {
    imports :: [ImportDirective1],
    from :: StringLiteral
  } deriving (Show, Eq, Ord)

data ImportDirective' a =
  ImportDirective' {
    imports' :: [ImportDirective1' a],
    from' :: StringLiteral' a,
    annot :: a
  } deriving (Show, Eq, Ord)

instance Annotated ImportDirective' a where
  ann (ImportDirective' _ _ a) = a

data ImportDirective1 =
  ImportDirective1 {
    name :: Import,
    as   :: Maybe Identifier
  } deriving (Show, Eq, Ord)
data Import = ImportAll | ImportId Identifier deriving (Show, Eq, Ord)


data ImportDirective1' a =
  ImportDirective1' {
    name' :: Import' a,
    as'   :: Maybe (Identifier' a)
  } deriving (Show, Eq, Ord)

data Import' a = ImportAll' a | ImportId' (Identifier' a) deriving (Show, Eq, Ord)
instance Annotated Import' a where
  ann (ImportAll' a) = a
  ann (ImportId' i) = ann i

instance Annotated ImportDirective1' a where
  ann (ImportDirective1' n _) = ann n
-------------------------------------------------------------------------------
-- ContractDefinition = ( 'contract' | 'library' | 'interface' ) Identifier
--                      ( 'is' InheritanceSpecifier (',' InheritanceSpecifier )* )?
--                      '{' ContractPart* '}'

data ContractDefinition =
  ContractDefinition {
    definitionType :: String,
    definitionName :: Identifier,
    isClause :: [InheritanceSpecifier],
    contractParts :: [ContractPart]
  } deriving (Show, Eq, Ord)

data ContractDefinition' a =
  ContractDefinition' {
    definitionType' :: String,
    definitionName' :: Identifier' a,
    isClause' :: [InheritanceSpecifier' a],
    contractParts' :: [ContractPart' a],
    annot :: a
  } deriving (Show, Eq, Ord)

instance Annotated ContractDefinition' a where
  ann (ContractDefinition' _ _ _ _ a) = a

-------------------------------------------------------------------------------
-- ContractPart
--    = 'using' Identifier 'for' ('*' | TypeName) ';'
--    | 'struct' Identifier '{' ( VariableDeclaration ';' (VariableDeclaration ';')* )? '}'
--    | 'modifier' Identifier ParameterList? Block
--    | 'constructor' ParameterList ( FunctionDefinitionTag )* ( ';' | Block )
--    | 'function' Identifier? ParameterList  ( FunctionDefinitionTag )* ( 'returns' ParameterList )? ( ';' | Block )
--    | 'enum' Identifier '{' EnumValue? (',' EnumValue)* '}'
--    | 'event' Identifier IndexedParameterList 'anonymous'? ';'
--    | StateVariableDeclaration

data ContractPart
  = ContractPartUsingForDeclaration Identifier (Maybe TypeName)
  | ContractPartStructDefinition Identifier [VariableDeclaration]
  | ContractPartModifierDefinition Identifier (Maybe ParameterList) Block
  | ContractPartConstructorDefinition ParameterList [FunctionDefinitionTag] (Maybe Block)
  | ContractPartFunctionDefinition (Maybe Identifier) ParameterList [FunctionDefinitionTag] (Maybe ParameterList) (Maybe Block)
  | ContractPartEnumDefinition Identifier [EnumValue]
  | ContractPartEventDefinition Identifier IndexedParameterList Bool
  | ContractPartStateVariableDeclaration StateVariableDeclaration
  deriving (Show, Eq, Ord)

data ContractPart' a
  = ContractPartUsingForDeclaration' (Identifier' a) (Maybe (TypeName' a)) a
  | ContractPartStructDefinition' (Identifier' a) [VariableDeclaration' a] a
  | ContractPartModifierDefinition' (Identifier' a) (Maybe (ParameterList' a)) (Block' a) a
  | ContractPartConstructorDefinition' (ParameterList' a) [FunctionDefinitionTag' a] (Maybe (Block' a)) a
  | ContractPartFunctionDefinition' (Maybe (Identifier' a)) (ParameterList' a) [FunctionDefinitionTag' a] (Maybe (ParameterList' a)) (Maybe (Block' a)) a
  | ContractPartEnumDefinition' (Identifier' a) [EnumValue' a] a
  | ContractPartEventDefinition' (Identifier' a) (IndexedParameterList' a) Bool a
  | ContractPartStateVariableDeclaration' (StateVariableDeclaration' a) a
  deriving (Show, Eq, Ord)

instance Annotated ContractPart' a where
  ann (ContractPartUsingForDeclaration' _ _ a) = a
  ann (ContractPartStructDefinition' _ _ a) = a
  ann (ContractPartModifierDefinition' _ _ _ a) = a
  ann (ContractPartConstructorDefinition' _ _ _ a) = a
  ann (ContractPartFunctionDefinition' _ _ _ _ _ a) = a
  ann (ContractPartEnumDefinition' _ _ a) = a
  ann (ContractPartEventDefinition' _ _ _ a) = a
  ann (ContractPartStateVariableDeclaration' _ a) = a

-------------------------------------------------------------------------------
-- StateVariableDeclaration = TypeName ( 'public' | 'internal' | 'private' | 'constant' )? Identifier ('=' Expression)? ';'

data StateVariableDeclaration = StateVariableDeclaration {
    typename :: TypeName,
    visibility :: [String],
    variableName :: Identifier,
    initialValue :: Maybe Expression
  } deriving (Show, Eq, Ord)

data StateVariableDeclaration' a = StateVariableDeclaration' {
    typename' :: TypeName' a,
    visibility' :: [String],
    variableName' :: Identifier' a,
    initialValue' :: Maybe (Expression' a),
    annot :: a
  } deriving (Show, Eq, Ord)

instance Annotated StateVariableDeclaration' a where
  ann (StateVariableDeclaration' _ _ _ _ a) = a
  
-------------------------------------------------------------------------------
-- InheritanceSpecifier = UserDefinedTypeName ( '(' Expression ( ',' Expression )* ')' )?

data InheritanceSpecifier =
  InheritanceSpecifier { userDefinedTypeName :: UserDefinedTypeName, inheritanceParameters :: [Expression] } deriving (Eq, Ord, Show)
data InheritanceSpecifier' a =
  InheritanceSpecifier' { userDefinedTypeName' :: UserDefinedTypeName' a, inheritanceParameters' :: [Expression' a], annot :: a} deriving (Eq, Ord, Show)

instance Annotated InheritanceSpecifier' a where
  ann (InheritanceSpecifier' _ _ a) = a
-------------------------------------------------------------------------------
-- ModifierInvocation = Identifier ( '(' ExpressionList? ')' )?

data ModifierInvocation =
  ModifierInvocation {
    modifierInvocationIdentifier :: Identifier,
    modifierInvocationParameters :: Maybe ExpressionList
  } deriving (Show, Eq, Ord)

data ModifierInvocation' a =
  ModifierInvocation' {
    modifierInvocationIdentifier' :: Identifier' a,
    modifierInvocationParameters' :: Maybe (ExpressionList' a),
    annot :: a
  } deriving (Show, Eq, Ord)

instance Annotated ModifierInvocation' a where
  ann (ModifierInvocation' _ _ a) = a

-------------------------------------------------------------------------------
-- FunctionDefinitionTag = ModifierInvocation | StateMutability | 'public' | 'internal' | 'private'

data FunctionDefinitionTag
  = FunctionDefinitionTagModifierInvocation ModifierInvocation
  | FunctionDefinitionTagStateMutability StateMutability
  | FunctionDefinitionTagPublic
  | FunctionDefinitionTagPrivate
  deriving (Show, Eq, Ord)

data FunctionDefinitionTag' a
  = FunctionDefinitionTagModifierInvocation' (ModifierInvocation' a)
  | FunctionDefinitionTagStateMutability' (StateMutability' a)
  | FunctionDefinitionTagPublic' a
  | FunctionDefinitionTagPrivate' a
  deriving (Show, Eq, Ord)

instance Annotated FunctionDefinitionTag' a where
  ann (FunctionDefinitionTagModifierInvocation' l) = ann l
  ann (FunctionDefinitionTagStateMutability' l) = ann l
  ann (FunctionDefinitionTagPublic' a) = a
  ann (FunctionDefinitionTagPrivate' a) = a

-------------------------------------------------------------------------------
-- EnumValue = Identifier

type EnumValue = Identifier
type EnumValue' = Identifier'


-------------------------------------------------------------------------------
-- IndexedParameterList =
--  '(' ( TypeName 'indexed'? Identifier? (',' TypeName 'indexed'? Identifier?)* )? ')'

newtype IndexedParameterList = IndexedParameterList [IndexedParameter] deriving (Eq, Ord, Show)
newtype IndexedParameterList' a = IndexedParameterList' [IndexedParameter' a] deriving (Eq, Ord, Show)

data IndexedParameter = IndexedParameter {
    indexedParameterType :: TypeName,
    indexedParameterIndexed :: Bool,
    indexedParameterIdentifier :: Maybe Identifier
  } deriving (Eq, Ord, Show)

data IndexedParameter' a = IndexedParameter' {
    indexedParameterType' :: TypeName' a,
    indexedParameterIndexed' :: Bool,
    indexedParameterIdentifier' :: Maybe (Identifier' a),
    annot :: a
  } deriving (Eq, Ord, Show)

instance Annotated IndexedParameter' a where
  ann (IndexedParameter' _ _ _ a) = a

-------------------------------------------------------------------------------
-- UntypedParameterList = '(' ( Identifier (',' Identifier)* )? ')'
-- Added for DEAs

newtype UntypedParameterList = UntypedParameterList { fromUntypedParameterList :: [Identifier] } deriving (Eq, Ord, Show)
newtype UntypedParameterList' a = UntypedParameterList' { fromUntypedParameterList :: [Identifier' a]} deriving (Eq, Ord, Show)
-------------------------------------------------------------------------------
-- ParameterList = '(' ( TypeName StorageLocation? Identifier? (',' TypeName StorageLocation? Identifier?)* )? ')'

newtype ParameterList = ParameterList [Parameter] deriving (Eq, Ord, Show)
newtype ParameterList' a = ParameterList' [Parameter' a] deriving (Eq, Ord, Show)

data Parameter = Parameter {
    parameterType :: TypeName,
    parameterStorageLocation :: Maybe StorageLocation,
    parameterIdentifier :: Maybe Identifier
  } deriving (Show, Eq, Ord)

data Parameter' a = Parameter' {
    parameterType' :: TypeName' a,
    parameterStorageLocation' :: Maybe (StorageLocation' a),
    parameterIdentifier' :: Maybe (Identifier' a),
    annot :: a
  } deriving (Show, Eq, Ord)

instance Annotated Parameter' a where
  ann (Parameter' _ _ _ a) = a

-------------------------------------------------------------------------------
-- TypeNameList = '(' ( TypeName (',' TypeName )* )? ')'

newtype TypeNameList = TypeNameList [TypeName] deriving (Eq, Ord, Show)

newtype TypeNameList' a = TypeNameList' [TypeName' a] deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- VariableDeclaration = TypeName StorageLocation? Identifier
data VariableDeclaration = VariableDeclaration {
    variableDeclarationType :: TypeName,
    variableDeclarationStorageLocation :: Maybe StorageLocation,
    variableDeclarationName :: Identifier
  } deriving (Eq, Ord, Show)

data VariableDeclaration' a = VariableDeclaration' {
    variableDeclarationType' :: TypeName' a,
    variableDeclarationStorageLocation' :: Maybe (StorageLocation' a),
    variableDeclarationName' :: Identifier' a,
    annot :: a
  } deriving (Eq, Ord, Show)  


instance Annotated VariableDeclaration' a where
  ann (VariableDeclaration' _ _ _ a) = a

-------------------------------------------------------------------------------
-- TypeName
--          = 'mapping' '(' ElementaryTypeName '=>' TypeName ')'
--          | ElementaryTypeName
--          | 'function' TypeNameList ( StateMutability )* ( 'returns' TypeNameList )?
--          | UserDefinedTypeName

--          | TypeName '[' Expression? ']'

data TypeName
  = TypeNameMapping ElementaryTypeName TypeName
  | TypeNameFunctionTypeName TypeNameList [StateMutability] (Maybe TypeNameList)
  | TypeNameElementaryTypeName ElementaryTypeName
  | TypeNameUserDefinedTypeName UserDefinedTypeName
  | TypeNameArrayTypeName TypeName (Maybe Expression)
  deriving (Eq, Ord, Show)

data TypeName' a
  = TypeNameMapping' (ElementaryTypeName' a) (TypeName' a) a
  | TypeNameFunctionTypeName' (TypeNameList' a) [StateMutability' a] (Maybe (TypeNameList' a)) a
  | TypeNameElementaryTypeName' (ElementaryTypeName' a) a
  | TypeNameUserDefinedTypeName' (UserDefinedTypeName' a) a
  | TypeNameArrayTypeName' (TypeName' a) (Maybe (Expression' a)) a
  deriving (Eq, Ord, Show)

instance Annotated TypeName' a where
  ann (TypeNameMapping' _ _ a) = a
  ann (TypeNameFunctionTypeName' _ _ _ a) = a
  ann (TypeNameElementaryTypeName' _ a) = a
  ann (TypeNameUserDefinedTypeName' _ a) = a
  ann (TypeNameArrayTypeName' _ _ a) = a

-------------------------------------------------------------------------------
-- UserDefinedTypeName = Identifier ( '.' Identifier )*

newtype UserDefinedTypeName = UserDefinedTypeName [Identifier] deriving (Eq, Ord, Show)

newtype UserDefinedTypeName' a = UserDefinedTypeName' [Identifier' a] deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- StorageLocation = 'memory' | 'storage' | 'calldata'

data StorageLocation = Memory | Storage | CallData deriving (Show, Eq, Ord)
data StorageLocation' a = StorageLocation' StorageLocation a deriving (Show, Eq, Ord)

instance Annotated StorageLocation' a where
  ann (StorageLocation' _ a) = a

-------------------------------------------------------------------------------
-- StateMutability = 'internal' | 'external' | 'pure' | 'constant' | 'view' | 'payable'

data StateMutability = Pure | Constant | View | Payable | Internal | External deriving (Eq, Ord, Show)

data StateMutability' a = StateMutability' StateMutability a deriving (Eq, Ord, Show)

instance Annotated StateMutability' a where
  ann (StateMutability' _ a) = a

-------------------------------------------------------------------------------
-- IdentifierList = '(' ( Identifier? ',' )* Identifier? ')'

newtype IdentifierList = IdentifierList [Identifier] deriving (Eq, Ord, Show)
newtype IdentifierList' a = IdentifierList' [Identifier' a] deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- Block = '{' Statement* '}'

newtype Block = Block [Statement] deriving (Eq, Ord, Show)
data Block' a = Block' [Statement' a] a deriving (Eq, Ord, Show)
instance Annotated Block' a where
  ann (Block' _ a) = a
-------------------------------------------------------------------------------
-- Statement = IfStatement | WhileStatement | ForStatement | Block | InlineAssemblyStatement |
--             ( DoWhileStatement | PlaceholderStatement | Continue | Break | Return |
--               Throw | SimpleStatement ) ';'
--
-- IfStatement = 'if' '(' Expression ')' Statement ( 'else' Statement )?
-- WhileStatement = 'while' '(' Expression ')' Statement
-- InlineAssemblyStatement = 'assembly' StringLiteral? InlineAssemblyBlock
-- ForStatement = 'for' '(' (SimpleStatement)? ';' (Expression)? ';' (Expression)? ')' Statement
--
-- DoWhileStatement = 'do' Statement 'while' '(' Expression ')'
-- PlaceholderStatement = '_'
-- Continue = 'continue'
-- Break = 'break'
-- Return = 'return' Expression?
-- Throw = 'throw'
-- EmitStatement = 'emit' Expression
-- SimpleStatement =
--    Expression | ('var' IdentifierList ( '=' Expression ) | VariableDeclaration ( '=' Expression )?
data Statement
  = IfStatement Expression Statement (Maybe Statement)
  | WhileStatement Expression Statement
  | InlineAssemblyStatement (Maybe StringLiteral) InlineAssemblyBlock
  | ForStatement (Maybe Statement, Maybe Expression, Maybe Expression) Statement
  | BlockStatement Block

  | DoWhileStatement Statement Expression
  | PlaceholderStatement
  | Continue
  | Break
  | Return (Maybe Expression)
  | Throw
  | EmitStatement Expression
  | SimpleStatementExpression Expression
  | SimpleStatementVariableList IdentifierList (Maybe Expression)
 -- | SimpleStatementVariableDeclaration VariableDeclaration (Maybe Expression)
  | SimpleStatementVariableDeclarationList [Maybe VariableDeclaration] [Expression]
  | SimpleStatementVariableAssignmentList [Maybe Identifier] [Expression]
  deriving (Eq, Ord, Show)

data Statement' a
  = IfStatement' (Expression' a) (Statement' a) (Maybe (Statement' a)) a
  | WhileStatement' (Expression' a) (Statement' a) a
  | InlineAssemblyStatement' (Maybe (StringLiteral' a)) InlineAssemblyBlock a
  | ForStatement' (Maybe (Statement' a), Maybe (Expression' a), Maybe (Expression' a)) (Statement' a) a
  | BlockStatement' (Block' a)

  | DoWhileStatement' (Statement' a) (Expression' a) a
  | PlaceholderStatement' a
  | Continue' a
  | Break' a
  | Return' (Maybe (Expression' a)) a
  | Throw' a
  | EmitStatement' (Expression' a) a
  | SimpleStatementExpression' (Expression' a) a
  | SimpleStatementVariableList' (IdentifierList' a) (Maybe (Expression' a)) a
 -- | SimpleStatementVariableDeclaration VariableDeclaration (Maybe Expression)
  | SimpleStatementVariableDeclarationList' [Maybe (VariableDeclaration' a)] [Expression' a] a
  | SimpleStatementVariableAssignmentList' [Maybe (Identifier' a)] [Expression' a] a
  deriving (Eq, Ord, Show)


instance Annotated Statement' a where
  ann (IfStatement' _ _ _ a) = a
  ann (WhileStatement' _ _ a) = a
  ann (InlineAssemblyStatement' _ _ a) = a
  ann (ForStatement' _ _ a) = a
  ann (BlockStatement' block) = ann block
  ann (DoWhileStatement' _ _ a) = a
  ann (PlaceholderStatement' a) = a
  ann (Continue' a) = a
  ann (Break' a) = a
  ann (Return' _ a) = a
  ann (Throw' a) = a
  ann (EmitStatement' _ a) = a
  ann (SimpleStatementExpression' _ a) = a
  ann (SimpleStatementVariableList' _ _ a) = a
  ann (SimpleStatementVariableDeclarationList' _ _ a) = a
  ann (SimpleStatementVariableAssignmentList' _ _ a) = a

-------------------------------------------------------------------------------
--  Precedence by order (see github.com/ethereum/solidity/pull/732)
-- Expression
--    = Expression ('++' | '--')
--    | Expression '[' Expression? ']'                                         -- index access
--    | '(' Expression ')'
--    | ('!' | '~' | 'delete' | '++' | '--' | '+' | '-') Expression
--    | Expression '**' Expression
--    | Expression ('*' | '/' | '%') Expression
--    | Expression ('+' | '-') Expression
--    | Expression ('<<' | '>>') Expression
--    | Expression '&' Expression
--    | Expression '^' Expression
--    | Expression '|' Expression
--    | Expression ('<' | '>' | '<=' | '>=') Expression
--    | Expression ('==' | '!=') Expression
--    | Expression '&&' Expression
--    | Expression '||' Expression
--    | Expression '?' Expression ':' Expression
--    | Expression ('=' | '|=' | '^=' | '&=' | '<<=' | '>>=' | '+=' | '-=' | '*=' | '/=' | '%=') Expression

--   | PrimaryExpression
--   | Expression '(' ('{' NameValueList? '}' | ExpressionList? ) ')'         -- function call
--   | Expression '.' Identifier                                              -- member access
--   | 'new' Typename

data Expression
  = Unary String Expression
  | Binary String Expression Expression
  | Ternary String Expression Expression Expression
  | FunctionCallNameValueList Expression (Maybe NameValueList)
  | FunctionCallExpressionList Expression (Maybe ExpressionList)
  | MemberAccess Expression Identifier
  | Literal PrimaryExpression
  | New TypeName
  deriving (Eq, Ord, Show)

data Expression' a
  = Unary' String (Expression' a) a
  | Binary' String (Expression' a) (Expression' a) a
  | Ternary' String (Expression' a) (Expression' a) (Expression' a) a
  | FunctionCallNameValueList' (Expression' a) (Maybe (NameValueList' a)) a
  | FunctionCallExpressionList' (Expression' a) (Maybe (ExpressionList' a)) a
  | MemberAccess' (Expression' a) (Identifier' a) a
  | Literal' (PrimaryExpression' a)
  | New' (TypeName' a) a
  deriving (Eq, Ord, Show)

instance Annotated Expression' a where
  ann (Unary' _ _ a) = a
  ann (Binary' _ _ _ a) = a
  ann (Ternary' _ _ _ _ a) = a
  ann (FunctionCallNameValueList' _ _ a) = a
  ann (FunctionCallExpressionList' _ _ a) = a
  ann (MemberAccess' _ _ a) = a
  ann (Literal' pe) = ann pe
  ann (New' _ a) = a

-------------------------------------------------------------------------------
-- PrimaryExpression = BooleanLiteral
--                   | NumberLiteral
--                   | HexLiteral
--                   | StringLiteral
--                   | TupleExpression
--                   | Identifier
--                   | ElementaryTypeNameExpression
data PrimaryExpression
  = PrimaryExpressionBooleanLiteral BooleanLiteral
  | PrimaryExpressionNumberLiteral NumberLiteral
  | PrimaryExpressionHexLiteral HexLiteral
  | PrimaryExpressionStringLiteral StringLiteral
  | PrimaryExpressionTupleExpression TupleExpression
  | PrimaryExpressionIdentifier Identifier
  | PrimaryExpressionElementaryTypeNameExpression ElementaryTypeNameExpression
  deriving (Eq, Ord, Show)

data PrimaryExpression' a
  = PrimaryExpressionBooleanLiteral' (BooleanLiteral' a)
  | PrimaryExpressionNumberLiteral' (NumberLiteral' a)
  | PrimaryExpressionHexLiteral' (HexLiteral' a)
  | PrimaryExpressionStringLiteral' (StringLiteral' a)
  | PrimaryExpressionTupleExpression' (TupleExpression' a)
  | PrimaryExpressionIdentifier' (Identifier' a)
  | PrimaryExpressionElementaryTypeNameExpression' (ElementaryTypeNameExpression' a)
  deriving (Eq, Ord, Show)

instance Annotated PrimaryExpression' a where
  ann (PrimaryExpressionBooleanLiteral' e) = ann e
  ann (PrimaryExpressionNumberLiteral' e) = ann e
  ann (PrimaryExpressionHexLiteral' e) = ann e
  ann (PrimaryExpressionStringLiteral' e) = ann e
  ann (PrimaryExpressionTupleExpression' e) = ann e
  ann (PrimaryExpressionIdentifier' e) = ann e
  ann (PrimaryExpressionElementaryTypeNameExpression' e) = ann e

-------------------------------------------------------------------------------
-- ExpressionList = Expression ( ',' Expression )*

newtype ExpressionList = ExpressionList { unExpressionList :: [Expression] } deriving (Eq, Ord, Show)

newtype ExpressionList' a = ExpressionList' { unExpressionList' :: [Expression' a]} deriving (Eq, Ord, Show)


-------------------------------------------------------------------------------
-- NameValueList = Identifier ':' Expression ( ',' Identifier ':' Expression )*

newtype NameValueList = NameValueList [(Identifier, Expression)] deriving (Show, Eq, Ord)

newtype NameValueList' a = NameValueList' [(Identifier' a, Expression' a)] deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- BooleanLiteral = 'true' | 'false'

newtype BooleanLiteral = BooleanLiteral String deriving (Eq, Ord, Show)

data BooleanLiteral' a = BooleanLiteral' String a deriving (Eq, Ord, Show)

instance Annotated BooleanLiteral' a where
  ann (BooleanLiteral' _ a) = a

-------------------------------------------------------------------------------
-- NumberLiteral = ( HexNumber | DecimalNumber ) (' ' NumberUnit)?

-- HexNumber = '0x' [0-9a-fA-F]+
-- DecimalNumber = [0-9]+

data NumberLiteral
  = NumberLiteralHex String (Maybe NumberUnit)
  | NumberLiteralDec String (Maybe NumberUnit)
 deriving (Eq, Ord, Show)

data NumberLiteral' a = NumberLiteral' NumberLiteral a deriving (Eq, Ord, Show)

instance Annotated NumberLiteral' a where
  ann (NumberLiteral' _ a) = a

-------------------------------------------------------------------------------
-- NumberUnit = 'wei' | 'szabo' | 'finney' | 'ether'
--           | 'seconds' | 'minutes' | 'hours' | 'days' | 'weeks' | 'years'

data NumberUnit
  = Wei | Szabo | Finney | Ether | Seconds | Minutes | Hours | Days | Weeks | Years
  deriving (Show, Eq, Ord) 

-- data NumberUnit' a = NumberUnit' NumberUnit a deriving (Show, Eq, Ord)
-------------------------------------------------------------------------------
-- HexLiteral = 'hex' ('"' ([0-9a-fA-F]{2})* '"' | '\'' ([0-9a-fA-F]{2})* '\'')

newtype HexLiteral = HexLiteral String deriving (Show, Eq, Ord)

data HexLiteral' a = HexLiteral' String a deriving (Show, Eq, Ord)

instance Annotated HexLiteral' a where
  ann (HexLiteral' _ a) = a
-------------------------------------------------------------------------------
-- StringLiteral = '"' ([^"\r\n\\] | '\\' .)* '"'

newtype StringLiteral = StringLiteral String deriving (Show, Eq, Ord)

data StringLiteral' a = StringLiteral' String a deriving (Show, Eq, Ord)

instance Annotated StringLiteral' a where
  ann (StringLiteral' _ a) = a

-------------------------------------------------------------------------------
-- Identifier = [a-zA-Z_$] [a-zA-Z_$0-9]*

newtype Identifier = Identifier { unIdentifier :: String } deriving (Eq, Ord)

instance Show Identifier where
  show i = "(Identifier \"" ++ unIdentifier i ++ "\")"

data Identifier' a = Identifier' { unIdentifier' :: String, annot :: a} deriving (Show, Eq, Ord)

instance Annotated Identifier' a where
  ann = annot
-- -------------------------------------------------------------------------------
-- TupleExpression = '(' ( Expression ( ',' Expression )*  )? ')'
--                 | '[' ( Expression ( ',' Expression )*  )? ']'

data TupleExpression
  = RoundBrackets [Expression]
  | SquareBrackets [Expression]
  deriving (Show, Eq, Ord)

data TupleExpression' a
  = RoundBrackets' [Expression' a] a
  | SquareBrackets' [Expression' a] a
  deriving (Show, Eq, Ord)

instance Annotated TupleExpression' a where
  ann (RoundBrackets' _ a) = a
  ann (SquareBrackets' _ a) = a
-- -------------------------------------------------------------------------------
-- ElementaryTypeNameExpression = ElementaryTypeName

type ElementaryTypeNameExpression = ElementaryTypeName

type ElementaryTypeNameExpression' a = ElementaryTypeName' a

-- -------------------------------------------------------------------------------
-- ElementaryTypeName = 'address' | 'bool' | 'string' | 'var'
--                    | Int | Uint | Byte | Fixed | Ufixed
--
-- Int = 'int' | 'int8' | 'int16' | 'int24' | 'int32' | 'int40' | 'int48' | 'int56' | 'int64' | 'int72' | 'int80' | 'int88' | 'int96' | 'int104' | 'int112' | 'int120' | 'int128' | 'int136' | 'int144' | 'int152' | 'int160' | 'int168' | 'int176' | 'int184' | 'int192' | 'int200' | 'int208' | 'int216' | 'int224' | 'int232' | 'int240' | 'int248' | 'int256'
-- Uint = 'uint' | 'uint8' | 'uint16' | 'uint24' | 'uint32' | 'uint40' | 'uint48' | 'uint56' | 'uint64' | 'uint72' | 'uint80' | 'uint88' | 'uint96' | 'uint104' | 'uint112' | 'uint120' | 'uint128' | 'uint136' | 'uint144' | 'uint152' | 'uint160' | 'uint168' | 'uint176' | 'uint184' | 'uint192' | 'uint200' | 'uint208' | 'uint216' | 'uint224' | 'uint232' | 'uint240' | 'uint248' | 'uint256'
-- Byte = 'byte' | 'bytes' | 'bytes1' | 'bytes2' | 'bytes3' | 'bytes4' | 'bytes5' | 'bytes6' | 'bytes7' | 'bytes8' | 'bytes9' | 'bytes10' | 'bytes11' | 'bytes12' | 'bytes13' | 'bytes14' | 'bytes15' | 'bytes16' | 'bytes17' | 'bytes18' | 'bytes19' | 'bytes20' | 'bytes21' | 'bytes22' | 'bytes23' | 'bytes24' | 'bytes25' | 'bytes26' | 'bytes27' | 'bytes28' | 'bytes29' | 'bytes30' | 'bytes31' | 'bytes32'
-- Fixed = 'fixed' | ( 'fixed' DecimalNumber 'x' DecimalNumber )
-- Ufixed = 'ufixed' | ( 'ufixed' DecimalNumber 'x' DecimalNumber )

data ElementaryTypeName
  = AddressPayableType | AddressType | BoolType | StringType | VarType
  | IntType (Maybe Integer) | UintType (Maybe Integer) | BytesType (Maybe Integer)
  | ByteType | FixedType (Maybe (Integer, Integer)) | UfixedType (Maybe (Integer, Integer))
  deriving (Eq, Ord, Show)

data ElementaryTypeName' a = ElementaryTypeName' ElementaryTypeName a deriving (Eq, Ord, Show)

instance Annotated ElementaryTypeName' a where
  ann (ElementaryTypeName' _ a) = a
-- -------------------------------------------------------------------------------
-- InlineAssemblyBlock = '{' AssemblyItem* '}'

newtype InlineAssemblyBlock = InlineAssemblyBlock [AssemblyItem] deriving (Eq, Ord, Show)

-- -------------------------------------------------------------------------------
-- AssemblyItem = Identifier | FunctionalAssemblyExpression | InlineAssemblyBlock | AssemblyLocalBinding | AssemblyAssignment | AssemblyLabel | NumberLiteral | StringLiteral | HexLiteral
-- AssemblyLabel = Identifier ':'
-- AssemblyLocalBinding = 'let' Identifier ':=' FunctionalAssemblyExpression
-- AssemblyAssignment = ( Identifier ':=' FunctionalAssemblyExpression ) | ( '=:' Identifier )

data AssemblyItem
  = AssemblyItemFunctionalAssemblyExpression FunctionalAssemblyExpression
  | AssemblyItemInlineAssemblyBlock InlineAssemblyBlock
  | AssemblyItemAssemblyLocalBinding Identifier FunctionalAssemblyExpression
  | AssemblyItemAssemblyAssignment Identifier FunctionalAssemblyExpression
  | AssemblyItemNumberLiteral NumberLiteral
  | AssemblyItemStringLiteral StringLiteral
  | AssemblyItemHexLiteral HexLiteral
  | AssemblyItemAssemblyLabel Identifier
  | AssemblyItemIdentifier Identifier
  deriving (Show, Eq, Ord)

-- -------------------------------------------------------------------------------
-- FunctionalAssemblyExpression = Identifier '(' AssemblyItem? ( ',' AssemblyItem )* ')'

data FunctionalAssemblyExpression = FunctionalAssemblyExpression Identifier [AssemblyItem] deriving (Show, Eq, Ord)

-- -------------------------------------------------------------------------------


typeParameterList :: UntypedParameterList -> ParameterList -> ParameterList
typeParameterList (UntypedParameterList ups) (ParameterList tps) =
  ParameterList $ zipWith
    (\parameter_name typed_parameter -> typed_parameter { parameterIdentifier = Just parameter_name })
    ups
    (tps ++
      repeat (
        Parameter {
          parameterType = TypeNameUserDefinedTypeName (UserDefinedTypeName [Identifier "undefined"]),
          parameterStorageLocation = Nothing,
          parameterIdentifier = Just (Identifier "undefined")
        }
      )
    )

untypeParameterList :: ParameterList -> UntypedParameterList
untypeParameterList (ParameterList ps) = UntypedParameterList $ map (fromJust . parameterIdentifier) ps

-- -------------------------------------------------------------------------------

addMemoryLocationToListOfParameters :: [Parameter] -> [Parameter]
addMemoryLocationToListOfParameters [] = []
addMemoryLocationToListOfParameters ((Parameter (TypeNameArrayTypeName t e) Nothing id):rest) = let newRest = (addMemoryLocationToListOfParameters rest)
                                                                                               in ((Parameter (TypeNameArrayTypeName t e) (Just Memory) id): newRest)
addMemoryLocationToListOfParameters ((Parameter (TypeNameElementaryTypeName StringType) Nothing id):rest) = let newRest = (addMemoryLocationToListOfParameters rest)
                                                                                                            in ((Parameter (TypeNameElementaryTypeName StringType) (Just Memory) id): newRest)
addMemoryLocationToListOfParameters ((Parameter (TypeNameElementaryTypeName (BytesType no)) Nothing id):rest) = let newRest = (addMemoryLocationToListOfParameters rest)
                                                                                                                in ((Parameter (TypeNameElementaryTypeName (BytesType no)) (Just Memory) id): newRest)
addMemoryLocationToListOfParameters (x:rest) = let newRest = (addMemoryLocationToListOfParameters rest)
                                                in (x:newRest)


addMemoryLocationToParametersList :: ParameterList -> ParameterList
addMemoryLocationToParametersList (ParameterList ps) = ParameterList (addMemoryLocationToListOfParameters ps)
