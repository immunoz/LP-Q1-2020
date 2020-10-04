myLength :: [Int] -> Int
--given a list of integers, returns its length
myLength [] 	= 0
myLength (x:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int
--given a non-empty list of integers, returns its maximal element
myMaximum a = auxMax (head a) (tail a)
	where
		auxMax h t
			|	t == []			= h
			|	h > head t 		= auxMax h (tail t) 
			|	otherwise		= auxMax (head t) (tail t)

average :: [Int] -> Float
--given a non-empty list of integers, re-turns its average.
average l = auxAverage (l) 0 0
	where
		auxAverage nums s n
			| nums == []	= fromIntegral (s) / fromIntegral(n)
			| otherwise		= auxAverage (tail nums) (s + (head nums)) (n+1)

myReverse :: [Int] -> [Int]
--Returns an array with its values in reverse order
myReverse [k] = [k]
myReverse k = last (k) : myReverse (init k)



buildPalindrome :: [Int] -> [Int]
--returns its palindrome that starts with the reserved list
buildPalindrome [] = []
buildPalindrome x = myReverse (x) ++ x ++ []

remove :: [Int] -> [Int] -> [Int]
--given a list of integers x and a list of integers y, returns x after having removed all the ocurrences of the elements in y.
remove [] _ = []
remove a [] = a
remove a (head:tail)
	| myLength b == 1		= remove' a head
	where remove' x y
		remove' [] _ = []
		remove' 


