module IR.Specs.Lexicals.Identifier where

newtype IIdentifier = Identifier {unIdentifier :: String} deriving (Show, Eq, Ord)
