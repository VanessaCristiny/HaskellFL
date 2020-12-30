module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

    data Bearing = North
                 | East
                 | South
                 | West
                 deriving (Eq, Show, Enum)

    data Robot = Robot Bearing (Int,Int) deriving (Eq, Show)

    succ North = East
    succ East  = South
    succ South = West
    succ West  = North

    pred North = West
    pred East  = North
    pred South = East
    pred West  = South

    foldl f z []     = z
    foldl f z (x:xs) = let z' = f z x 
                        in foldl f z' xs

    bearing :: Robot -> Bearing
    bearing (Robot dir pos) = dir

    coordinates :: Robot -> (Int, Int)
    coordinates (Robot dir pos) = pos

    mkRobot :: Bearing -> (Int, Int) -> Robot
    mkRobot direction coordinates = Robot direction coordinates

    turnLeft :: Bearing -> Bearing
    turnLeft direction 
            | direction == North = West
            | otherwise          = pred direction

    turnRight :: Bearing -> Bearing
    turnRight direction 
            | direction == West  = North
            | otherwise          = succ direction

    movement (Robot North (x, y)) 'A' = Robot North (x, y + 1)
    movement (Robot North (x, y)) 'R' = Robot (turnRight North) (x, y)
    movement (Robot North (x, y)) 'L' = Robot (turnLeft  North) (x, y)

    movement (Robot East (x, y)) 'A' = Robot East (x + 1, y)
    movement (Robot East (x, y)) 'R' = Robot (turnRight East) (x, y)
    movement (Robot East (x, y)) 'L' = Robot (turnLeft  East) (x, y)

    movement (Robot South (x, y)) 'A' = Robot South (x, y + 1)
    movement (Robot South (x, y)) 'R' = Robot (turnRight South) (x, y - 1)
    movement (Robot South (x, y)) 'L' = Robot (turnLeft  South) (x, y)

    movement (Robot West (x, y)) 'A' = Robot West (x, y) -- BUG
    movement (Robot West (x, y)) 'R' = Robot (turnRight West) (x, y)
    movement (Robot West (x, y)) 'L' = Robot (turnLeft  West) (x - 1, y)

    simulate :: Robot -> String -> Robot
    simulate robot mov = foldl movement robot mov
