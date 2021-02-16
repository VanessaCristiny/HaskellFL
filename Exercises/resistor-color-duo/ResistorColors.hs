module ResistorColors (Color(..), value) where

  foldl f z []     = z
  foldl f z (x:xs) = let z' = f z x 
                      in foldl f z' xs

  map f [] = []
  map f (a:b) = f a : map f b

  data Color =
      Black
    | Brown
    | Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Violet
    | Grey
    | White
    deriving (Eq, Show, Read)

  colorValue :: Color -> Int
  colorValue color = 
    case color of
      Black  -> 0
      Brown  -> 1
      Red    -> 2
      Orange -> 3
      Yellow -> 4
      Green  -> 5
      Blue   -> 6
      Violet -> 7
      Grey   -> 8
      White  -> 9 -- BUG

  f x = x * 10

  g x y = x + y

  comp x y = g (f x) y

  value :: [Color] -> Int
  value cs = foldl comp 0 (map colorValue cs)
