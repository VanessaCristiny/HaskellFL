module Main () where

	import Prelude hiding (map, dropWhile, break, words, concat)
	import Data.Char
	import Data.List hiding (map, dropWhile, break, words, concat)

	-- 1
	map _ [] = []
	map f (xh:xr) = f xh : map f xr

	-- 2
	dropWhile _ [] = []
	dropWhile p (xh:xr) = if p xh 
		then dropWhile p xr 
		else xh:xr

	-- 4
	break' _ [] = []
	break' p (xh:xr) = if p xh 
		then (xh: break' p xr) 
		else []
	break p l = (break' p l, dropWhile p l) -- BUG

	-- 7
	concat [] = []
	concat ([]:t) = concat(t)
	concat ((h1:t1):t) = h1:concat(t1:t)
