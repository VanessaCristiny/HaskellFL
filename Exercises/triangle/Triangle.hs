module Triangle (TriangleType(..), triangleType) where

  data TriangleType = Equilateral
                    | Isosceles
                    | Scalene
                    | Illegal
                    deriving (Eq, Show)

  filter :: (a -> Bool) -> [a] -> [a]
  filter _ [] = []
  filter f (x:xs)
    | f x       = x : (filter f xs)
    | otherwise = filter f xs

  quickSort :: (Ord a) => [a] -> [a]
  quickSort [] = []
  quickSort (x:xs)  = quickSort smaller ++ [x] ++ quickSort larger
      where smaller = filter (\y -> y <= x) xs
            larger  = filter (\y -> y > x) xs

  getPos :: [Int] -> Int -> Int
  getPos (x:(y:(z:zs))) n = case n of
    1 -> x
    2 -> y
    3 -> z

  triangleType :: (Num a, Eq a, Ord a) => a -> a -> a -> TriangleType
  triangleType a b c 
      | (((x + y) < z) || (x <= 0))   = Illegal -- BUG
      | (x == z)                      = Equilateral
      | (x == y) || (y == z)          = Isosceles
      | otherwise                     = Scalene
          where x = getPos (quickSort [a,b,c]) 1
                y = getPos (quickSort [a,b,c]) 2
                z = getPos (quickSort [a,b,c]) 3
