module Cli (optionsP, Options (..)) where

import Options.Applicative (auto, help, long, metavar, option, short, strArgument, switch)
import qualified Options.Applicative as Opt

data Options = Options
  { file :: String,
    lex :: Bool,
    parse :: Bool,
    codegen :: Bool
  }
  deriving (Show, Eq)

optionsP :: Opt.Parser Options
optionsP =
  Options
    <$> strArgument (help "the file to compile")
    <*> switch (long "lex" <> short 'l' <> help "Stop after lexing")
    <*> switch (long "parse" <> short 'p' <> help "Stop after parsing")
    <*> switch (long "codegen" <> short 'c' <> help "Stop after codegen")
