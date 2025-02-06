module Main where

import Cli (Options (..), optionsP)
import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc, (<**>))

data Token = LParen | RParen

compile :: Options -> IO ()
compile o@(Options file lex parse codegen) = print o

main :: IO ()
main = compile =<< execParser opts
  where
    opts =
      info
        (optionsP <**> helper)
        ( fullDesc
            <> header "milliliter - a minimal c compiler"
            <> progDesc "Milliliter is a tiny but full-featured C Compiler"
        )
