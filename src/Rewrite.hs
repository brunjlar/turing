-- | Core rewrite rule types and evaluation routines.
--
-- The 'Rewrite' module defines a minimal representation for rewrite rules
-- and exposes helpers to repeatedly apply them to an input sequence.
module Rewrite
  ( Rule (..)
  , Rules
  , trace
  ) where

-- | A single rewrite rule from a left-hand side to a right-hand side.
data Rule a = Rule ![a] ![a]
  deriving (Eq, Show)

-- | A collection of rewrite rules evaluated in order.
type Rules a = [Rule a]

-- | Drop the given prefix from a list, returning the remainder when it
-- matches.
withoutPrefix :: Eq a => [a] -> [a] -> Maybe [a]
withoutPrefix []       ys       = Just ys
withoutPrefix _        []       = Nothing
withoutPrefix (x : xs) (y : ys)
  | x == y                      = withoutPrefix xs ys
  | otherwise                   = Nothing

-- | Attempt to apply a single rule to the provided sequence.
applyRule :: Eq a => Rule a -> [a] -> Maybe [a]
applyRule r@(Rule lhs rhs) xs = case withoutPrefix lhs xs of
  Just ys -> Just (rhs ++ ys)
  Nothing -> case xs of
    []       -> Nothing
    (y : ys) -> (y :) <$> applyRule r ys

-- | Perform a single rewrite step using the first applicable rule.
step :: Eq a => Rules a -> [a] -> Maybe [a]
step []       _  = Nothing
step (r : rs) xs = case applyRule r xs of
  Just ys
    | ys /= xs  -> Just ys
    | otherwise -> Nothing
  Nothing       -> step rs xs

-- | Generate the (potentially infinite) sequence of successive rewrites
-- starting from the given input.
trace :: Eq a => Rules a -> [a] -> [[a]]
trace rules xs = xs : case step rules xs of
  Just ys -> trace rules ys
  Nothing -> []
