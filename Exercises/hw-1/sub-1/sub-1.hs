module Main () where
  import Prelude hiding (map)
  import Data.Char

  map f [] = []
  map f (a:b) = f a : map f b

  dropWhileClone p []          = []
  dropWhileClone p (x:xs)
       | p x       = dropWhileClone p xs
       | otherwise = x:xs

  splitWith p []     = []
  splitWith p x      = x1 : splitWith p x2
    where (x1 ,x2 ) = break p x

  isSpace s = if s == "" -- BUG
    then True 
    else False 

  break _ []      = ([],[])
  break p (a:x)
    | p a         = ([],a:x)
    | otherwise   = (a:x1,x2)
    where (x1,x2) = break p x

  wordsClone s = if s' == [] then [] else (word : wordsClone rest)
    where s' = dropWhileClone isSpace s
          (word, rest) = break isSpace s'

  concatClone [] = []
  concatClone ([]:vs) = concatClone vs
  concatClone ((x:xs):vs) = x:concatClone (xs:vs) 
