module Lex
  ( longestMatchFromStart,
    isFullMatch,
    flexibleMatchStart,
    identMeta,
    intMeta,
    voidMeta,
    constMeta,
    lparenMeta,
    rparenMeta,
    lbraceMeta,
    rbraceMeta,
    semicolonMeta,
    tokenMeta,
    TokenMeta (..),
    Token (..),
    Keyword (..),
    tokenize,
  )
where

import Data.Maybe (fromJust, isJust)
import Data.Text (pack, strip, unpack)
import Text.Regex.TDFA (getAllMatches, (=~))

data Keyword = KInt | KVoid | KReturn deriving (Show, Eq)

data Token = Identifier String | Keyword Keyword | Constant Int | LParen | RParen | LBrace | RBrace | Semicolon deriving (Show, Eq)

data TokenMeta = TokenMeta {re :: String, name :: String} deriving (Show, Eq)

identMeta = TokenMeta {re = "[a-zA-Z_]([a-zA-Z0-9_])*([:space:])*", name = "ident"}

intMeta = TokenMeta {re = "int\\b", name = "int"}

voidMeta = TokenMeta {re = "void\\b", name = "void"}

constMeta = TokenMeta {re = "[0-9]+", name = "const"}

lparenMeta = TokenMeta {re = "\\(", name = "lparen"}

rparenMeta = TokenMeta {re = "\\)", name = "rparen"}

lbraceMeta = TokenMeta {re = "{", name = "lbrace"}

rbraceMeta = TokenMeta {re = "}", name = "rbrace"}

semicolonMeta = TokenMeta {re = ";", name = "semicolon"}

tokenMeta = [identMeta, intMeta, voidMeta, constMeta, lparenMeta, rparenMeta, lbraceMeta, rbraceMeta, semicolonMeta]

data MatchToken = MatchToken String Token

instance Eq MatchToken where
  (MatchToken m1 t1) == (MatchToken m2 t2) = m1 == m2 && t1 == t2

instance Ord MatchToken where
  (MatchToken m1 _) <= (MatchToken m2 _) = length m1 <= length m2

isFullMatch :: String -> String -> Bool
isFullMatch re str =
  let matches = getAllMatches (str =~ re) :: [(Int, Int)]; len = length matches; identLen = length str
   in ((len == 1) && (identLen == snd (head matches)))

flexibleMatchStart :: String -> String -> Maybe String
flexibleMatchStart re str =
  let matches = getAllMatches (str =~ re) :: [(Int, Int)]
   in case matches of
        [] -> Nothing
        (start, len) : _ -> if start == 0 then Just $ slice 0 (len - 1) str else Nothing
  where
    slice :: Int -> Int -> [a] -> [a]
    slice from to = take (to - from + 1)

longestMatchFromStart :: String -> Either String (Token, Int)
longestMatchFromStart input = case longestMatch of
  Just (tok, len) -> Right (tok, len)
  Nothing -> Left $ "No token matched from start of string: " ++ input
  where
    longestMatch = maximum' [m | tok <- tokenMeta, Just m <- [go tok input]]

    go :: TokenMeta -> String -> Maybe (Token, Int)
    go (TokenMeta re name) input = case match of
      Just match -> case name of
        "ident" -> Just (Identifier match, length match)
        "const" -> Just (Constant $ read match, length match)
        "void" -> Just (Keyword KVoid, length match)
        "int" -> Just (Keyword KInt, length match)
        "return" -> Just (Keyword KReturn, length match)
        "lparen" -> Just (LParen, length match)
        "rparen" -> Just (RParen, length match)
        "lbrace" -> Just (LBrace, length match)
        "rbrace" -> Just (RBrace, length match)
        "semicolon" -> Just (Semicolon, length match)
        _ -> Nothing
      Nothing -> Nothing
      where
        match = flexibleMatchStart re input

    maximum' :: [(Token, Int)] -> Maybe (Token, Int)
    maximum' [] = Nothing
    maximum' lst = Just $ foldr1 (\f@(tok1, len1) s@(tok2, len2) -> if len1 > len2 then f else s) lst

tokenize :: String -> Either (String, [Token]) [Token]
tokenize input = tokenize' [] input
  where
    tokenize' :: [Token] -> String -> Either (String, [Token]) [Token]
    tokenize' lst "" = Right lst
    tokenize' lst input = case longestMatchFromStart $ unpack $ strip $ pack input of
      Left e -> Left (e, lst)
      Right (tok, len) -> tokenize' (lst ++ [tok]) (drop len (unpack $ strip $ pack input))
