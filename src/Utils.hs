{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Utils where

import Data.Char as C
import Data.Word
import Numeric (showHex)
import qualified Data.Text as T

-- annotation: placeholder, to be instantiated
newtype Ann = Ann { unAnn :: Maybe String } deriving (Eq, Ord, Show)
nil :: Ann 
nil = Ann Nothing

toLower :: String -> String
toLower = map C.toLower


hexChar :: Char -> Word8
hexChar ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'A' = 10
    | ch == 'a' = 10
    | ch == 'B' = 11
    | ch == 'b' = 11
    | ch == 'C' = 12
    | ch == 'c' = 12
    | ch == 'D' = 13
    | ch == 'd' = 13
    | ch == 'E' = 14
    | ch == 'e' = 14
    | ch == 'F' = 15
    | ch == 'f' = 15
    | otherwise     = 0

parseHex :: String -> [Word8]
parseHex [] = []
parseHex [_] = error "Invalid hex: length of the hex literal must be an even number" 
parseHex (x:y:xs) = hexChar x * 16 + hexChar y: parseHex xs


showHexWithPadded :: (Integral n, Show n) => n -> String
showHexWithPadded n = if n > 15 then showHex n "" else "0" ++ showHex n ""

headWord :: String -> String
headWord = head . words

trim :: String -> String
trim = T.unpack . T.strip . T.pack