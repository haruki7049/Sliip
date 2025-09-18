{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative (Parser, ParserInfo, execParser, help, helper, info, long, metavar, progDesc, short, strOption, switch, (<**>))
import Sliip.Evaluator (eval)
import Text.Parsec (parse)

main :: IO ()
main = execParser argumentParserInfo >>= run

run :: CLIArgument -> IO ()
run CLIArgument {..} = do
  eval script

newtype CLIArgument = CLIArgument
  { script :: String
  }
  deriving (Read, Show)

cliParser :: Parser CLIArgument
cliParser =
  CLIArgument
    <$> strOption (long "script" <> short 's' <> metavar "SCRIPT" <> help "Write your Sliip script here")

withInfo :: Parser a -> String -> ParserInfo a
withInfo p = info (p <**> helper) . progDesc

argumentParserInfo :: ParserInfo CLIArgument
argumentParserInfo = cliParser `withInfo` "A joke Lisp interpreter"
