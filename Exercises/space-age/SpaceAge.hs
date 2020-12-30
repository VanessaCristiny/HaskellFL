module SpaceAge where

    data Planet = Mercury
                | Venus
                | Earth
                | Mars
                | Jupiter
                | Saturn
                | Uranus
                | Neptune

    orbitalPeriod :: Planet -> Float
    orbitalPeriod planet =
        let 
            earthYear = 31557600
        in 
            case planet of 
                Mercury -> 0.24084670 * earthYear
                Venus   -> 0.61519725 * earthYear -- BUG
                Earth   -> 1.00000000 * earthYear
                Mars    -> 1.88081580 * earthYear
                Jupiter -> 11.8626150 * earthYear
                Saturn  -> 29.4474980 * earthYear
                Uranus  -> 84.0168460 * earthYear
                Neptune -> 164.791320 * earthYear

    ageOn :: Planet -> Float -> Float
    ageOn planet age = age / (orbitalPeriod planet) 
