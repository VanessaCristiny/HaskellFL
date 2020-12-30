module School (School, 
               add, 
               empty, 
               grade, 
               sorted) where

  import Data.Maybe (fromMaybe)
  import Data.List (sortBy, sort)

  data Maybe a = Nothing | Just a 

  fromMaybe x (Nothing) = x
  fromMaybe x (Just y) = y

  fst (x,y) = x

  max x y = if x > y
    then x
    else y

  min x y = if x < y
    then x
    else y

  bubbleSort :: (Ord a) => [a] -> [a]
  bubbleSort [] = []
  bubbleSort [x] = [x]
  bubbleSort (x:y:xs) = if sorted2 thisSort 
    then thisSort 
    else bubbleSort thisSort
      where thisSort = (min x y) : bubbleSort ((max x y):xs)

  sorted2 :: (Ord a) => [a] -> Bool
  sorted2 [] = True
  sorted2 [x] = True
  sorted2 (x:y:xs) = if x <= y 
    then sorted2 (y:xs) 
    else False 

  empty = []

  find _ [] = 
      Nothing
  find x ((y1, y2):ys) 
      | x == y1 = 
          Just ys -- BUG
      | otherwise = 
          find x ys  

  remove _ [] = 
      empty
  remove y (x:xs) 
      | y == fst x = 
          xs
      | otherwise  = 
          x : remove y xs 

  add gradeNum student school = 
      case (find gradeNum school) of 
          Nothing -> 
              (gradeNum, [student]) : school
          Just students -> 
              (gradeNum, (student : students)) : 
               remove gradeNum school

  grade gradeNum school = 
      fromMaybe [] (find gradeNum school)  
