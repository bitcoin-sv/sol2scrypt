module Cli where

import Options.Applicative

data Options
  = Transpile {cDest :: FilePath, cSource :: Maybe FilePath, cLogToFile :: Bool, cForceOutput :: Bool}
  | Version
  deriving (Show)

sourceParser :: Parser (Maybe FilePath)
sourceParser = optional $ argument str $ metavar "FILE" <> help "Source file path. Take source file content from stdin if none given"

destParser :: Parser FilePath
destParser = strOption $ long "output-dir" <> short 'o' <> metavar "OUTPUTDIR" <> value "." <> help "Output directory if given, default to current directory"

logToFileParser :: Parser Bool
logToFileParser = switch $ long "log" <> short 'L' <> help "Whether to output transpile log in json format"

forceOutputParser :: Parser Bool
forceOutputParser = switch $ long "force" <> short 'F' <> help "Whether to output transpile result fileeven got errors"

commandsParser :: Parser Options
commandsParser =
  hsubparser
    ( command
        "transpile"
        ( info
            (Transpile <$> destParser <*> sourceParser <*> logToFileParser <*> forceOutputParser)
            (progDesc "Transpile Solidity FILE to sCrypt")
        )
        <> command
          "version"
          ( info
              (pure Version)
              (progDesc "Show version")
          )
    )

cli :: ParserInfo Options
cli = info (commandsParser <**> helper) (fullDesc <> progDesc "Solidity to sCrypt Transpiler")
