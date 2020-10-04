eql :: [Int] -> [Int] -> Bool
-- Tells wether two lists of integers are equal.
eql x y
	| length x /= length y	= False
	| otherwise 			= and $ zipWith (==) x y

prod :: [Int] -> Int
-- Returns the product of a list of integers.
prod k = foldl (*) 1 k

prodOfEvens :: [Int] -> Int
--returns the product of all even numbers of a list of integers.
prodOfEvens k = foldl (*) 1 (filter even k)

powersOf2 :: [Int]
powersOf2 = iterate (2*) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct a b = sum (zipWith (*) a b)