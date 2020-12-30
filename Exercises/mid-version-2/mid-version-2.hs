module Main where
	mid x y z = if y < z
		then if x < y 
			then y 
			else if x < z
				then y -- BUG
				else z
		else if x > y
			then y 
			else if x > z 
				then x
				else z
