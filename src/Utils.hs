{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Utils where

import Data.Char as C
import Data.Word
import Numeric (showHex)

toLower :: String -> String
toLower = map C.toLower


charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

parseHex :: String -> [Word8]
parseHex [] = []
parseHex (x:y:xs) = (charToWord8 x - 48) * 16 + (charToWord8 y - 48): parseHex xs

showHexPadded :: (Show n, Integral n) => Int -> n -> ShowS
-- ^ showHex of n padded with leading zeros if necessary to fill d digits
showHexPadded d n = showString (replicate (d - sigDigits n) '0') . showHex n  where
  sigDigits 0 = 1
  sigDigits n' = truncate (logBase 16 $ fromIntegral n' :: Double) + 1


showHexWithPadded :: (Integral n, Show n) => n -> String 

showHexWithPadded n = if n > 15 then showHex n "" else "0" ++ showHex n ""

