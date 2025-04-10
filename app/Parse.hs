{-# LANGUAGE LambdaCase #-}

module Parse
  ( Expr (..),
    Stmt (..),
    Func (..),
    AST (..),
    Parser (..),
  )
where

import Control.Monad.State (State, evalState, runState, state)
import Debug.Trace (trace)
import Lex (Token)

data Expr = Constant Int deriving (Eq, Show)

data Stmt = Return Expr | If Expr Stmt (Maybe Stmt) deriving (Eq, Show)

data Func = String Stmt deriving (Eq, Show)

data AST = Program AST | Func | Stmt | Expr deriving (Eq, Show)

type Parser = State [Token] (Maybe AST)

nextToken :: [Token] -> (Maybe Token, [Token])
nextToken [] = (Nothing, [])
nextToken (t : ts) = (Just t, ts)

data ParseError
  = UnexpectedEOF [Token]
  | InvalidExpr [Token]
  | InvalidStmt [Token]
  | InvalidConst [Token]
  deriving (Show, Eq)

nextNTokens :: Int -> [Token] -> Either ParseError ([Token], [Token])
nextNTokens 0 xx = Right ([], xx)
nextNTokens _ [] = Left $ UnexpectedEOF []
nextNTokens n xx@(x : xs) = case nextNTokens (n - 1) xs of
  Right (xs', xx') -> Right (x : xs', xx')
  Left ~(UnexpectedEOF partial) -> Left $ UnexpectedEOF xx

parseExpr :: [Token] -> (Maybe AST, [Token])
parseExpr tokens = undefined

parseFunc :: [Token] -> (Maybe AST, [Token])
parseFunc = undefined

parse :: [Token] -> Either (String, AST) AST
parse ts = undefined

-- takeNTokens :: Int -> [Token] -> Either ParseError ([Token], [Token])
-- takeNTokens 0 tokens = Right ([], tokens)
-- takeNTokens _ [] = Left (UnexpectedEOF [])
-- takeNTokens n tt@(t : ts) = case takeNTokens (n - 1) ts of
--   Right (ts', tt') -> Right (t : ts', tt')
--   e -> e
