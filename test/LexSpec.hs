module LexSpec where

import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = do
  describe "check that the lexer works" $ do
    it "checks that `matchConst works" $ do
      putStrLn "asdf"
    -- matchConst "0" `shouldBe` Just (Const 0)
    -- matchConst "1;" `shouldBe` Just (Const 1)
    -- matchConst "01" `shouldBe` Nothing

    it "checks that `matchMinusMinus` works" $ do
      putStrLn "asdf"

-- matchMinusMinus "--" `shouldBe` Just TMinusMinus
-- matchMinusMinus "--;" `shouldBe` Just TMinusMinus
-- matchMinusMinus "- -" `shouldBe` Nothing
