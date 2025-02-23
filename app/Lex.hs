module Lex (Token (..), matchIdentOrKeyword, tokenize, lstrip) where

import Control.Monad.State (State, runState, state)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Text.Regex.TDFA (getAllMatches, (=~))

data Token
  = Const Int
  | Identifier String
  | Keyword String
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Semicolon
  | SingleLineComment String
  | MultiLineComment String
  deriving (Show, Eq)

data LexError = UnexpectedEOF deriving (Show, Eq)

length' :: Token -> Int
length' tok = case tok of
  Const val -> length $ show val
  Identifier ident -> length ident
  Keyword kw -> length kw
  LParen -> 1
  RParen -> 1
  LBrace -> 1
  RBrace -> 1
  Semicolon -> 1
  SingleLineComment comment -> length comment
  MultiLineComment comment -> length comment

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

longest :: [Token] -> Maybe Token
longest [] = Nothing
longest lst = Just $ maximumBy (comparing length') lst

matchIdentOrKeyword,
  matchConst,
  matchLParen,
  matchRParen,
  matchLBrace,
  matchRBrace,
  matchSemicolon,
  matchSingleLineComment,
  matchMultiLineComment ::
    String -> Maybe Token
matchIdentOrKeyword input =
  flexibleMatchStart "[a-zA-Z_]([a-zA-Z0-9_])*([:space:])*" input >>= \match ->
    case matchKeyword match of
      (Just kw) -> Just kw
      _ -> Just $ Identifier match
  where
    matchKeyword :: String -> Maybe Token
    matchKeyword ident = longest [m | f <- [matchVoid, matchInt], Just m <- [f ident]]

    matchVoid ident = flexibleMatchStart "void\\b" ident >>= \match -> Just $ Keyword "void"
    matchInt ident = flexibleMatchStart "int\\b" ident >>= \match -> Just $ Keyword "int"
matchConst input = flexibleMatchStart "[0-9]+\\b" input >>= \match -> Just $ Const $ read match
matchLParen input = flexibleMatchStart "\\(" input >> Just LParen
matchRParen input = flexibleMatchStart "\\)" input >> Just RParen
matchLBrace input = flexibleMatchStart "{" input >> Just LBrace
matchRBrace input = flexibleMatchStart "}" input >> Just RBrace
matchSemicolon input = flexibleMatchStart ";" input >> Just Semicolon
matchSingleLineComment input = flexibleMatchStart "//.*" input >>= \match -> Just $ SingleLineComment match
matchMultiLineComment input = flexibleMatchStart "[:space:]*/\\*.*\\*/" input >>= \match -> Just $ MultiLineComment match

lstrip :: String -> String
lstrip "" = ""
lstrip input = dropWhile isWhitespace input
  where
    isWhitespace input = input == ' ' || input == '\t' || input == '\n'

tokenize :: String -> Either (LexError, [Token]) [Token]
tokenize input = go [] (lstrip input)
  where
    go :: [Token] -> String -> Either (LexError, [Token]) [Token]
    go lst "" = Right lst
    go lst input = case munch input of
      Just tok -> go (lst ++ [tok]) (lstrip $ drop (length' tok) input)
      Nothing -> Left (UnexpectedEOF, lst)

    munch s =
      longest
        [ m
          | f <-
              [ matchIdentOrKeyword,
                matchConst,
                matchLParen,
                matchRParen,
                matchLBrace,
                matchRBrace,
                matchSemicolon,
                matchSingleLineComment,
                matchMultiLineComment
              ],
            Just m <- [f s]
        ]
