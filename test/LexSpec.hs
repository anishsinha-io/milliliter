module LexSpec where

import Data.Either (isRight)
import Lex
  ( Keyword (..),
    Token (..),
    identMeta,
    isFullMatch,
    longestMatchFromStart,
    re,
    tokenize,
  )
import Test.Hspec (Spec, before, describe, it)

lexTestCases :: [String]
lexTestCases =
  [ "int main(void) { return 0; }"
  ]

identifierTestCases :: ([String], [String])
identifierTestCases = (["myvariable", "myVariable", "my_variable", "my_v4riabl3"], ["my#variable", "1my_variable", "my$variable"])

longestMatchTestCases :: [(String, Token)]
longestMatchTestCases =
  [ ("intidentifier", Identifier "intidentifier"),
    ("int", Keyword KInt),
    ("()", LParen),
    (")(", RParen),
    ("{}", LBrace),
    ("}{", RBrace),
    (";int x = 5", Semicolon),
    (";int;", Semicolon)
  ]

spec :: Spec
spec = do
  describe "the regular expressions for all tokens work correctly" $ do
    before (return identifierTestCases) $ do
      it "checks the identifier regex" $ \(valid, invalid) -> do
        let acceptsValid = all (isFullMatch $ re identMeta) valid
        let rejectsInvalid = not $ any (isFullMatch $ re identMeta) invalid

        acceptsValid && rejectsInvalid

  describe "check lexing methods" $ do
    before (return longestMatchTestCases) $ do
      it "checks `longestMatchFromStart` for correctness" $ \cs -> do
        and [matchedTok == expected | (input, expected) <- cs, (Right (matchedTok, _)) <- [longestMatchFromStart input]]

  describe "test lexer" $ do
    before (return lexTestCases) $ do
      it "checks whether the tokenizer works" $ \cs -> do
        let prog1 = head cs
        isRight $ tokenize prog1
