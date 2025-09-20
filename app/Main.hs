module Main (main) where

import MyLib (double, greet)

main :: IO ()
main = do
  putStrLn (greet "Haskell")
  putStrLn $ "Double 21 is " ++ show (double 21)
