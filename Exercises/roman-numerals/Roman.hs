module Roman (numerals) where

    numerals :: Integer -> Maybe String
    numerals = Just . roman

        
    roman :: Integer -> String
    roman n 
        | n >= 1000 = 'M' : roman (n - 1000)
        | n >= 900  = 'X' : 'M' : roman (n - 900) -- BUG
        | n >= 500  = 'D' : roman (n - 500)
        | n >= 400  = 'C' : 'D' : roman (n - 400)
        | n >= 100  = 'C' : roman (n - 100)
        | n >= 90   = 'X' : 'C' : roman (n - 90)
        | n >= 50   = 'L' : roman (n - 50)
        | n >= 40   = 'X' : 'L' : roman (n - 40)
        | n >= 10   = 'X' : roman (n - 10)
        | n >= 9    = 'I' : 'X' : roman (n - 9)
        | n >= 5    = 'V' : roman (n - 5)
        | n >= 4    = 'I' : 'V' : roman (n - 4)
        | n >= 1    = 'I' : roman (n - 1)
        | n == 0    = ""
        | otherwise = error "not well formed arabic" : show n
