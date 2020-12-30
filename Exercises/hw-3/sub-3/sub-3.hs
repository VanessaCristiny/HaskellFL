module Main where

	f x y
		| x < y 	= show x
		| otherwise = show y

	flip :: (a -> b -> c) -> b -> a -> c
	flip f x y = f x y -- BUG

	f (g,h) (x,y) = let { z = h y } in (undefined, h (g,x))

	hi f g a = f (g a)

	myUnzip [] = ([], []) -- Defaults to a pair of empty lists, not null
	myUnzip xs = (map fst xs, map snd xs)

	scanl :: (a -> b -> a) -> a -> [b] -> [a]
	scanl f q (x:xs)     = if xs == []
		then [q]
		else q : (scanl f (f q x) xs)
