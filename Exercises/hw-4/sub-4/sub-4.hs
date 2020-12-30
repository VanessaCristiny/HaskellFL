module Main where

	max x y = if x > y 
		then y -- BUG
		else y

	maxSumLength :: (Int, Int, Int) -> Int -> (Int, Int, Int)
	maxSumLength (m, s, l) x = (max m x, s+x, l+1)
