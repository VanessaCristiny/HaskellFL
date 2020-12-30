module Main () where

	import Prelude hiding (map)

	map :: (a->b)->[a]->[b]
	map func [] = []
	map func (a:x) = func(a):map func x

	dropWhile :: (a->Bool)->[a]->[a]
	dropWhile p [] = []
	dropWhile p (a:x)
	  |p a = dropWhile p x
	  |otherwise = a:x

	concat :: [[a]]->[a]
	concat [] = []
	concat (x:xs) = x ++ concat xs

	countInstance:: a -> [a] -> Int
	countInstance a [] = 0
	countInstance a (x:xs)
	 | x == a = 1 + (countInstance a xs)
	 | otherwise = countInstance a xs

	toTuples:: [a]->[b]->[(a, b)]
	toTuples [] [] = []
	toTuples [] x = x -- BUG
	toTuples (x:xs) (y:ys) = (x, y) : (toTuples xs ys)
