module Lex
  ( Token (..),
    tokenize,
    -- lstrip,
  )
where

import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Data.Text (pack, stripStart, unpack)
import Text.Regex.TDFA ((=~))

data Token
  = TConst Int
  | TIdentifier String
  | TKeyword String
  | TKeywordInt
  | TKeywordVoid
  | TKeywordFloat
  | TKeywordReturn
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TSemicolon
  | TSingleLineComment String
  | TMultiLineComment String
  | TMinusMinus
  | TPlusPlus
  | TAdd
  | TSub
  | TDeref
  | TMult
  | TDiv
  | TMod
  deriving (Show, Eq)

lstrip = unpack . stripStart . pack

matchKeyword,
  matchIdent,
  matchConst,
  matchLParen,
  matchRParen,
  matchLBrace,
  matchRBrace,
  matchSemicolon,
  matchSingleLineComment,
  matchMultiLineComment ::
    String -> (Maybe Token, String)
matchKeyword s = result
  where
    match :: String -> Token -> (Maybe Token, String)
    match re tok = case s =~ re :: (String, String, String) of
      ("", match, rest) -> (Just tok, rest)
      _ -> (Nothing, s)

    fns = [match "int\\b" TKeywordInt, match "void\\b" TKeywordVoid, match "float\\b" TKeywordFloat, match "return\\b" TKeywordReturn]

    result = case [(x, y) | (Just x, y) <- sortBy (\a b -> compare (length (snd a)) (length (snd b))) fns] of
      [] -> (Nothing, s)
      ((x, y) : _) -> (Just x, lstrip y)
matchIdent s = result
  where
    match :: String -> (Maybe Token, String)
    match re = case s =~ re :: (String, String, String) of
      ("", match, rest) -> (Just $ TIdentifier match, lstrip rest)
      _ -> (Nothing, s)

    result = case matchKeyword s of
      (Just kw, rest) -> (Nothing, s)
      (Nothing, _) ->
        let (mi, r) = match "[a-zA-Z_]([a-zA-Z0-9_])*([:space:])*"
         in case mi of
              Just ident -> (Just ident, r)
              Nothing -> (Nothing, s)
matchConst s = case s =~ "[0-9]+\\b" :: (String, String, String) of
  ("", match, rest) -> (Just $ TConst (read match), lstrip rest)
  _ -> (Nothing, s)
matchLParen s = case s =~ "\\(" :: (String, String, String) of
  ("", match, rest) -> (Just TLParen, lstrip rest)
  _ -> (Nothing, s)
matchRParen s = case s =~ "\\)" :: (String, String, String) of
  ("", match, rest) -> (Just TRParen, lstrip rest)
  _ -> (Nothing, s)
matchLBrace s = case s =~ "{" :: (String, String, String) of
  ("", match, rest) -> (Just TLBrace, lstrip rest)
  _ -> (Nothing, s)
matchRBrace s = case s =~ "}" :: (String, String, String) of
  ("", match, rest) -> (Just TLBrace, lstrip rest)
  _ -> (Nothing, s)
matchSemicolon s = case s =~ ";" :: (String, String, String) of
  ("", match, rest) -> (Just TSemicolon, lstrip rest)
  _ -> (Nothing, s)
matchSingleLineComment s = case s =~ "//.*" :: (String, String, String) of
  ("", match, rest) -> (Just $ TSingleLineComment match, lstrip rest)
  _ -> (Nothing, s)
matchMultiLineComment s = case s =~ "[:space:]*/\\*.*\\*/" :: (String, String, String) of
  ("", match, rest) -> (Just $ TMultiLineComment match, lstrip rest)
  _ -> (Nothing, s)

data LexError = UnrecognizedToken String deriving (Eq, Show)

tokenize :: String -> Either LexError [Token]
tokenize s = result
  where
    matches =
      let s' = lstrip s
       in [ matchKeyword s',
            matchIdent s',
            matchConst s',
            matchLParen s',
            matchRParen s',
            matchLBrace s',
            matchRBrace s',
            matchSemicolon s',
            matchSingleLineComment s',
            matchMultiLineComment s'
          ]

    match = case [(x, y) | (Just x, y) <- sortBy (\a b -> compare (length (snd a)) (length (snd b))) matches] of
      [] -> (Nothing, s)
      ((x, y) : _) -> (Just x, lstrip y)

    result = case match of
      (Just tok, "") -> Right [tok]
      (Just tok, rest) -> case tokenize rest of
        (Right tokens) -> Right (tok : tokens)
        e -> e
      (_, rest) -> Left (UnrecognizedToken rest)

inject :: (String -> (Maybe Token, String)) -> ((Maybe Token, String) -> (Maybe Token, String))
inject f (_, s) = f s
