module Parse where

import Lex (Token)

data Expr = Constant Int

data Stmt = Return Expr | If Expr Stmt (Maybe Stmt)

data Func = String Stmt

data AST = Program | Func | Stmt | Expr

parseExpr :: [Token] -> Either String AST
parseExpr tokens = undefined
  where
    x : xs = tokens

parse :: [Token] -> AST
parse = undefined
