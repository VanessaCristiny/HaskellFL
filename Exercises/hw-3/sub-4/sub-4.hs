module Main where

   f x y
     | x < y      = show x 
     | otherwise  = show y

   pair :: (a -> b, a -> c) -> a -> (b, c)
   pair (f, g) x = (f x, g x)

   myUnzip :: [(a, b)] -> ([a], [b]) 
   myUnzip [] = ([], [])
   myUnzip ((a, b):xs) = (a : n, b : n)
         where n = myUnzip xs -- BUG
