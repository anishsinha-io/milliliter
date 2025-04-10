module Main where

import qualified Cli (Options (..), optionsP)
import Control.Monad.State (evalState, runState)
import Lex (tokenize)
import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc, (<**>))

compile :: Cli.Options -> IO ()
compile o@(Cli.Options file stopAfterLex stopAfterParse stopAfterCodegen) =
  case (stopAfterLex, stopAfterParse, stopAfterCodegen) of
    (True, False, False) -> do
      program <- readFile file
      case tokenize program of
        Left e -> do
          error $ show e
        Right tokenstream -> print tokenstream
    -- (False, True, False) -> do
    --   program <- readFile file
    --   case tokenize program of
    --     Left e -> do
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

longestUniqueSubstring :: String -> Int
longestUniqueSubstring [] = 0
longestUniqueSubstring (x : xs) = go [x] xs
  where
    go :: String -> String -> Int
    go s [] = length s
    go s yy@(y : ys) =
      let s' = takeWhile (/= y) ys
       in if s /= s'
            then let s'' = drop (length s') s in go (y : s'') ys
            else go (s ++ [y]) ys
