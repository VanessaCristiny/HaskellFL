module Main where
	mid x y z = if y < (z - 1) -- BUG
		then if x < y 
			then y 
			else if x < z
				then x
				else z
		else if x > y
			then y 
			else if x > z 
				then x
				else z
