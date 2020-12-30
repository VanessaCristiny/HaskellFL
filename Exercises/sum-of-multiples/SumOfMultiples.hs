module SumOfMultiples (sumOfMultiples) where

    import Data.List(nub)

    filter :: (a -> Bool) -> [a] -> [a]
    filter _ [] = []
    filter f (x:xs)
      | f x       = x : (filter f xs)
      | otherwise = filter f xs

    nub :: (Eq a) => [a] -> [a]
    nub [] = []
    nub (x:xs) = x : nub (filter (\y -> y /= x) xs)

    sum :: [Int] -> Int
    sum [] = 0
    sum (x:xs) = x + sum xs

    appendFactor :: (Integral a) => a -> a -> a -> [a]
    appendFactor factor limit index 
        | (factor * index) > limit = [] -- BUG
        | factor == 0 = []
        | otherwise = 
            (factor * index) : 
            appendFactor factor limit (index + 1)

    distinctFactors :: (Integral a) => [a] -> a -> [a]
    distinctFactors [] limit = []
    distinctFactors (x:xs) limit = 
        nub ((distinctFactors xs limit) ++ (appendFactor x limit 1) )

    sumOfMultiples :: [Integer] -> Integer -> Integer
    sumOfMultiples [] limit = 0
    sumOfMultiples factors limit = sum (distinctFactors factors limit)
