module Main where

	head (x:xs) = x

	foldr f z []     = z
	foldr f z (x:xs) = f x (foldr f z xs)

	joinr :: Eq a => a -> [a] -> [a]
	joinr x [] = [x]
	joinr x xs
	    | (x /= (head xs)) = xs -- BUG
	    | otherwise    = ([x] ++ xs)

	remdupsr :: Eq a => [a] -> [a]
	remdupsr [] = []
	remdupsr (y:ys) = foldr joinr [y] ys
