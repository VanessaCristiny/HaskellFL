module Luhn (isValid) where

	import Data.Char (digitToInt)

	digitToInt n = case n of
		"0" -> 0
		"1" -> 1
		"2" -> 2
		"3" -> 3
		"4" -> 4
		"5" -> 5
		"6" -> 6
		"7" -> 7
		"8" -> 8
		"9" -> 9

	filter :: (a -> Bool) -> [a] -> [a]
	filter _ [] = []
	filter f (x:xs)
		| f x       = x : (filter f xs)
		| otherwise = filter f xs

	mod x y = let { res = (x / y) } 
		in (x - (res * y))

	map f [] = []
	map f (a:b) = f a : map f b

	reverse [] a = a
	reverse (x:xs) a = reverse xs (x:a)

	luhn []  = 0
	luhn [x] = x
	luhn (x0:(x1:xs)) = ((x0 + (2 * x1)) - 
		(if x1 > 5 -- BUG
			then 9 
			else 0)) 
		+ (luhn xs)

	checksum xs = mod (luhn (reverse (map digitToInt xs) [])) 10

	isValid :: String -> Bool
	isValid n = case (filter (\x -> x /= " ") n) of
		[] -> False
		xs -> (checksum xs) == 0
