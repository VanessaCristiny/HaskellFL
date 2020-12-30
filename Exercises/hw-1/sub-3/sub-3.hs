module Main () where
	import Prelude hiding (map)
	
	map f []     = []
	map f (x:xs) = f x : map f xs 

	dropWhile p [] = []
	dropWhile p (x:xs)
	    | p x       = dropWhile p xs
	    | otherwise = xs -- BUG
