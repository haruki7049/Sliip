{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Main
Description : Command-line interface for the Sliip Lisp interpreter
Copyright   : (c) Sliip Contributors
License     : MIT
Maintainer  : haruki7049
Stability   : experimental

This module provides the command-line interface for the Sliip Lisp interpreter.
It handles argument parsing and invokes the evaluator on the specified Lisp source file.
-}
module Main where

import Options.Applicative (Parser, ParserInfo, execParser, help, helper, info, long, metavar, progDesc, short, strArgument, switch, (<**>))
import Sliip.Evaluator (eval)
import Text.Parsec (parse)

-- | Main entry point for the Sliip interpreter.
--
-- Parses command-line arguments and runs the interpreter on the specified file.
main :: IO ()
main = execParser argumentParserInfo >>= run

-- | Execute the Sliip interpreter with the given command-line arguments.
--
-- Reads the source file specified in the arguments and evaluates it.
run :: CLIArgument -> IO ()
run CLIArgument {..} = do
  script <- readFile filepath
  eval script

-- | Command-line arguments for the Sliip interpreter.
newtype CLIArgument = CLIArgument
  { filepath :: String -- ^ Path to the Lisp source file to evaluate
  }
  deriving (Read, Show)

-- | Parser for command-line arguments.
cliParser :: Parser CLIArgument
cliParser =
  CLIArgument
    <$> strArgument (metavar "FILEPATH")

-- | Attach a help message to a parser.
withInfo :: Parser a -> String -> ParserInfo a
withInfo p = info (p <**> helper) . progDesc

-- | Complete parser information for the command-line interface.
argumentParserInfo :: ParserInfo CLIArgument
argumentParserInfo = cliParser `withInfo` "A joke Lisp interpreter"
