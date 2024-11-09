module OptParse where

import Data.Maybe
import Options.Applicative

data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath
  deriving (Show)

data SingleInput
  = Stdin
  | InputFile FilePath
  deriving (Show)

data SingleOutput
  = StdOut
  | OutputFile FilePath
  deriving (Show)

pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
  where
    parser =
      strOption
        (long "input" <> short 'i' <> metavar "FILE" <> help "Input file")

pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> parser
  where
    parser =
      strOption
        (long "output" <> short 'o' <> metavar "FILE" <> help "Output file")

pSingleInput :: Parser SingleInput
pSingleInput = fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser SingleOutput
pSingleOutput = fromMaybe StdOut <$> optional pOutputFile

pConvertSingle :: Parser Options
pConvertSingle =
  ConvertSingle <$> pSingleInput <*> pSingleOutput

pInputDir :: Parser FilePath
pInputDir =
  strOption
    (long "input" <> short 'i' <> metavar "DIRECTORY" <> help "Input directory")

pOutputDir :: Parser FilePath
pOutputDir =
  strOption
    (long "output" <> short 'o' <> metavar "DIRECTORY" <> help "Output directory")

pConvertDir :: Parser Options
pConvertDir = ConvertDir <$> pInputDir <*> pOutputDir

pConvertSingleInfo :: ParserInfo Options
pConvertSingleInfo =
  info
    (helper <*> pConvertSingle)
    (progDesc "Convert a single markup source to html")

pConvertSingleCommand :: Mod CommandFields Options
pConvertSingleCommand =
  command "convert" pConvertSingleInfo

pOptions :: Parser Options
pOptions =
  subparser
    ( command
        "convert"
        (info (helper <*> pConvertSingle) (progDesc "Convert a single markup source to html"))
        <> command
          "convert-dir"
          (info (helper <*> pConvertDir) (progDesc "Convert a directory of markup files to html"))
    )

parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts =
  info
    (helper <*> pOptions)
    ( fullDesc
        <> header "hs-blog-gen - a static blog generator"
        <> progDesc "Convert markup files or directories to html"
    )
