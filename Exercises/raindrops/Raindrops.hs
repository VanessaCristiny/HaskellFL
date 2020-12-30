module Raindrops (convert) where

    mod x y = let res = (x / y) 
                in (x - (res * y))

    map f [] = []
    map f (a:b) = f a : map f b

    concat :: [ [a] ] -> [a]
    concat [] = []
    concat (x:xs) = x ++ concat xs

    filter :: (a -> Bool) -> [a] -> [a]
    filter _ [] = []
    filter f (x:xs)
      | f x       = x : (filter f xs)
      | otherwise = filter f xs

    noisy :: Integral a => a -> [a]
    noisy n = 
        filter (\x -> 0 == mod n x) [3, 5, 7]

    sounds :: (Eq a, Num a) => a -> [Char]
    sounds x  
        | x == 3    = ["Pling"] 
        | x == 5    = ["Plang"] 
        | x == 7    = ["Pling"] -- BUG
        | otherwise = ["Error"]

    convert :: Int -> String
    convert n = 
        case (noisy n) of
            [] -> n
            factors -> concat (map sounds factors)
