flatten :: [[Int]] -> [Int]
-- Flattens a list of lists of integers in a list of integers.
flatten k = foldl (++) [] k

myLength :: String -> Int
-- Returns the length of a string.
myLength s = foldl (countChar) 0 s
	where
		countChar :: Int -> Char -> Int
		countChar k _ = k + 1

myReverse :: [Int] -> [Int]
-- Reverses a list of integers.
myReverse k = foldr (flip :) [] k