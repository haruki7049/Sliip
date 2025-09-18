{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative (Parser, ParserInfo, execParser, help, helper, info, long, metavar, progDesc, short, strArgument, switch, (<**>))
import Sliip.Evaluator (eval)
import Text.Parsec (parse)

main :: IO ()
main = execParser argumentParserInfo >>= run

run :: CLIArgument -> IO ()
run CLIArgument {..} = do
  script <- readFile filepath
  eval script

newtype CLIArgument = CLIArgument
  { filepath :: String
  }
  deriving (Read, Show)

cliParser :: Parser CLIArgument
cliParser =
  CLIArgument
    <$> strArgument (metavar "FILEPATH")

withInfo :: Parser a -> String -> ParserInfo a
withInfo p = info (p <**> helper) . progDesc

argumentParserInfo :: ParserInfo CLIArgument
argumentParserInfo = cliParser `withInfo` "A joke Lisp interpreter"
