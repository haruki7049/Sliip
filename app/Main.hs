{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Sliip (programs)
import Text.Parsec (parse)
import Options.Applicative (Parser, ParserInfo, switch, info, progDesc, helper, (<**>), help, short, long, metavar, strOption, execParser)

main :: IO ()
main = execParser argumentParserInfo >>= run

run :: CLIArgument -> IO ()
run CLIArgument {..} = do
  case parse Sliip.programs "" script of
    Left err -> print err
    Right x -> print x

data CLIArgument = CLIArgument
  { script :: String
  } deriving (Read, Show)

cliParser :: Parser CLIArgument
cliParser = CLIArgument
  <$> strOption (long "script" <> short 's' <> metavar "SCRIPT" <> help "Write your Sliip script here")

withInfo :: Parser a -> String -> ParserInfo a
withInfo p = info (p <**> helper) . progDesc

argumentParserInfo :: ParserInfo CLIArgument
argumentParserInfo = cliParser `withInfo` "A joke Lisp interpreter"
