module IR.Specs.Lexicals.Identifier where

newtype IIdentifier = Identifier {unIdentifier :: String} deriving (Eq, Ord)

instance Show IIdentifier where
  show i = "(Identifier \"" ++ unIdentifier i ++ "\")"

reservedKeywords :: [String]
reservedKeywords =
  [ "contract",
    "constructor",
    "public",
    "static",
    "if",
    "else",
    "require",
    "exit",
    "true",
    "false",
    "bool",
    "int",
    "bytes",
    "loop",
    "function",
    "return",
    "new",
    "import",
    "this",
    "auto",
    "struct",
    "library",
    "const",
    "type",
    "state",
    "PrivKey",
    "PubKey",
    "Ripemd160",
    "Sha1",
    "Sha256",
    "Sig",
    "SigHashType",
    "OpCodeType",
    "SigHashPreimage",
    "asm"
  ]