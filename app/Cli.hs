module Cli where

import Options.Applicative

data Options
  = Transpile {cDest :: FilePath, cSource :: Maybe FilePath}
  | Version
  deriving (Show)

sourceParser :: Parser (Maybe FilePath)
sourceParser = optional $ argument str $ metavar "FILE" <> help "Source file path. Take source file content from stdin if none given"

destParser :: Parser FilePath
destParser = strOption $ long "output-dir" <> short 'o' <> metavar "OUTPUTDIR" <> value "." <> help "Output directory if given, default to current directory"

commandsParser :: Parser Options
commandsParser =
  hsubparser
    ( command
        "transpile"
        ( info
            (Transpile <$> destParser <*> sourceParser)
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
