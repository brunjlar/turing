module MyLib (greet, double) where

-- | Produce a friendly greeting for the provided name.
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

-- | Double an integer. Defined separately for readability and testability.
double :: Int -> Int
double n = n * 2
