module Cli where

import Options.Applicative

data Options
  = Transpile {cDest :: Maybe FilePath, cSource :: Maybe FilePath, cForceOutput :: Bool}
  | Version
  deriving (Show)

sourceParser :: Parser (Maybe FilePath)
sourceParser = optional $ argument str $ metavar "FILE" <> help "Source file path. Take source file content from stdin if none given"

destParser :: Parser (Maybe FilePath)
destParser = optional $ strOption $ long "output-dir" <> short 'o' <> metavar "OUTPUTDIR" <> help "Output directory, output to stdout if none given"

forceOutputParser :: Parser Bool
forceOutputParser = switch $ long "force" <> short 'F' <> help "Whether to output scrypt code despite errors"

commandsParser :: Parser Options
commandsParser =
  hsubparser
    ( command
        "transpile"
        ( info
            (Transpile <$> destParser <*> sourceParser <*> forceOutputParser)
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
