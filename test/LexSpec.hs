module LexSpec where

import Lex (lstrip, matchIdentOrKeyword, tokenize)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = do
  describe "make sure the lexer works" $ do
    it "checks that`matchIdentOrKeyword` works correctly" $ do
      print $ matchIdentOrKeyword "int main("
      print $ matchIdentOrKeyword "main("

      print $ lstrip "   asdf asd fasd f"

      print $ tokenize "int main() { return 0; }"

      print $ tokenize "int main(void) {return"
      print $ tokenize "   int   main    (  void)  {   return  0 ; }"

--       let acceptsValid = all (isFullMatch $ re identMeta) valid
--       let rejectsInvalid = not $ any (isFullMatch $ re identMeta) invalid
--
--       acceptsValid && rejectsInvalid
--
-- describe "check lexing methods" $ do
--   before (return longestMatchTestCases) $ do
--     it "checks `longestMatchFromStart` for correctness" $ \cs -> do
--       and [matchedTok == expected | (input, expected) <- cs, (Right (matchedTok, _)) <- [longestMatchFromStart input]]
--
-- describe "test lexer" $ do
--   before (return lexTestCases) $ do
--     it "checks whether the tokenizer works" $ \cs -> do
--       let prog1 = head cs
--       isRight $ tokenize prog1
