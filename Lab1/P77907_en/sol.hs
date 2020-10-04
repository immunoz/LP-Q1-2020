absValue :: Int -> Int
-- Given an integer number it returns its absolute vale
absValue n
    | n >= 0     =  n
    | otherwise  = -n

power :: Int -> Int -> Int
-- Given an integer x and p, returns the p-th power of x
power _ 0 = 1 
power x n
	| even n = y * y
	| otherwise = y * y * x
	where
		y = power x n_half
		n_half = div n 2

isPrime :: Int -> Bool
isPrime n
	|  n <= 2 	 = if n == 2 then True else False
	|  otherwise = isPrimeAux 2
	where
		isPrimeAux z 
			| z == n-1		 = True
			| mod n z == 0 	 = False
			| otherwise      = isPrimeAux (z+1)

slowFib :: Int -> Int
-- n needs to be greater or equal than 2
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib(n - 1) + slowFib (n - 2)

quickFib :: Int -> Int
-- n needs to be greater or equal than 2
quickFib 0 = 0
quickFib 1 = 1
quickFib 2 = 1
quickFib n
	| mod n 2 == 0		= (2 * quickFib (h-1) + quickFib h) * quickFib h
	| otherwise 		= (quickFib h * quickFib h) + (quickFib (h-1) * quickFib (h-1))
	where
		h = if mod n 2 == 0 then div n 2 else div (n+1) 2
