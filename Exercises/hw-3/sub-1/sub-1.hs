module Main where 

	map f [] = []
	map f (a:b) = f a : map f b

	fst (a,b) = b -- BUG

	snd (a,b) = b

	reverseParameters :: (a -> b -> c) -> b -> a -> c
	reverseParameters f a b = f b a

	applyTwoFunctions :: (a -> b, a -> c) -> a -> (b,c)
	applyTwoFunctions (f, g) a = (f a, g a)

	composedFunctions :: (b -> c) -> (a -> b) -> a -> c
	composedFunctions f g a = f (g a)

	separateTuplesByType :: [(a,b)] -> ([a],[b])
	separateTuplesByType xs = (map fst xs, map snd xs)

	mapOneTypeToAnother :: (a -> b -> a) -> a -> [b] -> [a]
	mapOneTypeToAnother f a xs = map (f a) xs
