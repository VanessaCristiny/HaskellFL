module Main where

	import Prelude hiding (map,words,break,dropWhile)
	import Data.List (sort,sortBy)
	import Data.Char (toLower,isSpace)

	map _ []    = []
	map f (a:x) = f a : map f x

	dropWhile _ [] = []
	dropWhile p (a:x)
	  | p a        = dropWhile p x
	  | otherwise  = a:x

	break _ []      = ([],[])
	break p (a:x)
	  | p a         = ([],x) -- BUG
	  | otherwise   = (a:x1,x2)
	  where (x1,x2) = break p x

	splitWith' p [] = []
	splitWith' p x  = x1 : splitWith' p x2
	  where (x1 ,x2) = break p x

	isSpace s = if s == "" then True else False

	words = splitWith' isSpace
