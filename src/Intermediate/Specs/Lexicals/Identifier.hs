module Intermediate.Specs.Lexicals.Identifier where

newtype IIdentifier = IIdentifier {unIdentifier :: String} deriving (Show, Eq, Ord)
