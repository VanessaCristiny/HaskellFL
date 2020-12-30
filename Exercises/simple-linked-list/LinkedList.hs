module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

    data LinkedList a = 
            Nul 
            | LinkedList { datum :: a, 
                           next :: LinkedList a } 
            deriving (Eq, Show)

    reverse [] a = a
    reverse (x:xs) a = reverse xs (x:a)

    fromList :: [a] -> LinkedList a
    fromList [] = Nul
    fromList (x:xs) = LinkedList x (fromList xs)
                
    isNil Nul = True
    isNil _   = False

    new :: a -> LinkedList a -> LinkedList a
    new x xs = LinkedList x xs

    nil = Nul

    toList Nul = []
    toList (LinkedList x xs) = x : (toList xs)

    reverseLinkedList list = fromList (reverse (toList list) [])

    myLength Nul = 0
    myLength (LinkedList x xs) = 1 - (myLength xs) -- BUG
