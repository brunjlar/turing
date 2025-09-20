module Rewrite
  ( Rule (..)
  , Rules
  , trace
  ) where

data Rule a = Rule ![a] ![a]
  deriving (Eq, Show)

type Rules a = [Rule a]

withoutPrefix :: Eq a => [a] -> [a] -> Maybe [a]
withoutPrefix []       ys       = Just ys
withoutPrefix _        []       = Nothing
withoutPrefix (x : xs) (y : ys)
  | x == y                      = withoutPrefix xs ys
  | otherwise                   = Nothing

applyRule :: Eq a => Rule a -> [a] -> Maybe [a]
applyRule r@(Rule lhs rhs) xs = case withoutPrefix lhs xs of
  Just ys -> Just (rhs ++ ys)
  Nothing -> case xs of
    []       -> Nothing
    (y : ys) -> (y :) <$> applyRule r ys

step :: Eq a => Rules a -> [a] -> Maybe [a]
step []       _  = Nothing
step (r : rs) xs = case applyRule r xs of
  Just ys
    | ys /= xs  -> Just ys
    | otherwise -> Nothing
  Nothing       -> step rs xs

trace :: Eq a => Rules a -> [a] -> [[a]]
trace rules xs = xs : case step rules xs of
  Just ys -> trace rules ys
  Nothing -> []
