module Main where

import Transpiler

main :: IO ()
main = do
  let solProgram = ""
  scryptCode <- transpile solProgram
  -- putStrLn $ "Transpile `" ++ solProgram ++ "` to `" ++ scryptCode ++ "`"
  return ()
