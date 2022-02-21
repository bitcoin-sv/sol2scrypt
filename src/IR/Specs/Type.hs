module IR.Specs.Type where


data IType
  = ElementaryType IElementaryTypeName
  | BuiltinType String
  | UserDefinedType String
  | Array IType Int
  | Mapping IType IType
  deriving (Show, Eq, Ord)

data IElementaryTypeName 
  = Bool
  | Int
  | Bytes
  | String
  | Address
  | Any
  deriving (Show, Eq, Ord)