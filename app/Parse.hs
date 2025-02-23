{-# LANGUAGE LambdaCase #-}

module Parse
  ( Expr (..),
    Stmt (..),
    Func (..),
    AST (..),
    Parser (..),
    nextToken,
  )
where

import Control.Monad.State (State, evalState, runState, state)
import Debug.Trace (trace)
import GHC.IO (unsafePerformIO)
import Lex (Token)

data Expr = Constant Int deriving (Eq, Show)

data Stmt = Return Expr | If Expr Stmt (Maybe Stmt) deriving (Eq, Show)

data Func = String Stmt deriving (Eq, Show)

data AST = Nil | Program AST | Func | Stmt | Expr deriving (Eq, Show)

type Parser = State [Token] (Maybe AST)

nextToken :: State [Token] (Maybe Token)
nextToken = state $ \case [] -> (Nothing, []); (x : xs) -> (Just x, xs)

-- parseExpr :: Parser
-- parseExpr = state $ \s ->

parse :: [Token] -> Either (String, AST) AST
parse ts = undefined
