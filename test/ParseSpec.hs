module ParseSpec where

import Control.Monad.State (MonadState (state), runState)
import Data.Either (fromRight)
-- import Lex
--   ( tokenize,
--   )
-- import Parse (nextToken)
import Test.Hspec (Spec, before, describe, it)

-- parseProgram :: Parser AST
-- parseProgram = do
--   tok <- nextToken
--
--   print tok
--   return (Program Nil)

spec :: Spec
spec = do
  describe "the parsing works correctly" $ do
    it "checks whether the parsing works" $ \tokens -> do
      print "asdf"

-- before (return $ fromRight [] (tokenize "int main(void) { return 0; }")) $ do
--   it "checks whether the parsing works" $ \tokens -> do
--     let result =
--           runState
--             ( do
--                 a1 <- nextToken
--                 a2 <- nextToken
--                 return [a1, a2]
--             )
--             tokens
--     --
--     -- print result
--     print result

-- print result
