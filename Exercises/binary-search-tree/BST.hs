module BST (BST, bstLeft, bstRight, bstValue, empty, 
            fromList, insert, singleton, toList) where

    data BST a = Empty | BST (BST a) a (BST a) 
                 deriving (Eq, Show, Foldable)

    data Maybe a = Just a | Nothing

    foldl f z []     = z
    foldl f z (x:xs) = let z' = f z x 
                        in foldl f z' xs

    bstLeft tree = let (BST left _ _) = tree
                    in Just left

    bstRight tree = 
        let 
            (BST _ _ right) = tree
        in 
            Just right

    bstValue Empty = Nothing
    bstValue tree = 
        let
            (BST _ val _) = tree
        in 
            Just val

    empty = Empty

    fromList :: Ord a => [a] -> BST a
    fromList xs = 
        foldl' (flip insert) empty xs

    singleton :: a -> BST a
    singleton x = 
        BST Empty x Empty

    insert :: Ord a => a -> BST a -> BST a
    insert x Empty = singleton x
    insert x (BST left val right) 
        | x < val  =  -- BUG
            BST (insert x left) val right
        | otherwise =
            BST left val (insert x right)    

    toList Empty = 
        []
    toList tree = 
        let 
            (BST left val right) = tree
        in 
            (toList left) ++ 
            [val] ++ 
            (toList right)
