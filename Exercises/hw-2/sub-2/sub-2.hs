module Main where 
    import Prelude hiding (id)
    import Data.Char

    data Pessoa = Pessoa {nome::String, idade::Integer, id::String}

    foldr f z []     = z
    foldr f z (x:xs) = f x (foldr f z xs)

    foldl f z []     = z
    foldl f z (x:xs) = let z' = f z x 
                        in foldl f z' xs
    concat [] = []
    concat (x:xs) = x ++ (concat xs)

    head (x:xs) = x

    length xs = foldr (\_ n -> 1 + n) 0 xs

    idade (Pessoa n i id) = i

    -- 6
    remdups l = foldr remdupsReducer [] l
    remdupsReducer a b = if (length b > 0) && (a == head b) -- BUG
        then b 
        else a:b
