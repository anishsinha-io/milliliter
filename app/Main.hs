module Main where

-- /* '1foo' is not a valid token, because identifier can't start with digits. */
-- int main(void) {
--     return 1foo;
-- }

import Cli (Options (..), optionsP)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Lex (tokenize)
import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc, (<**>))

compile :: Options -> IO ()
compile o@(Options file lex parse codegen) =
  case (lex, parse, codegen) of
    (True, False, False) -> do
      program <- readFile file
      case tokenize program of
        Left (e, tokenstream) -> do
          print tokenstream
          error e
        Right tokenstream -> print tokenstream
    (_, _, _) -> error "bad option"

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
