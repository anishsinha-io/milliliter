module Main where

import qualified Cli (Options (..), optionsP)
import Control.Monad.State (evalState, runState)
import Lex (tokenize)
import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc, (<**>))
import Parse (nextToken)

compile :: Cli.Options -> IO ()
compile o@(Cli.Options file stopAfterLex stopAfterParse stopAfterCodegen) =
  case (stopAfterLex, stopAfterParse, stopAfterCodegen) of
    (True, False, False) -> do
      program <- readFile file
      case tokenize program of
        Left (e, tokenstream) -> do
          print tokenstream
          error $ show e
        Right tokenstream -> print tokenstream
    -- (False, True, False) -> do
    --   program <- readFile file
    --   case tokenize program of
    --     Left (e, tokenstream) -> do
    --       print tokenstream
    --       error e
    --     Right tokenstream -> let p = parse tokenstream in print p
    (_, _, _) -> error "bad option"

main :: IO ()
main = compile =<< execParser opts
  where
    opts =
      info
        (Cli.optionsP <**> helper)
        ( fullDesc
            <> header "milliliter - a minimal c compiler"
            <> progDesc "Milliliter is a tiny but full-featured C Compiler"
        )
