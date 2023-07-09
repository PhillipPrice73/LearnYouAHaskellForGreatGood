{- 
Chapter 4
Hello Recursion!
-}

-- fibonacci n returns the nth fibonacci number
fibonacci :: Integer -> Integer
fibonacci x
    | x < 0 = error "Need to pass a value greater than 0."
    | x == 0 = 0
    | x == 1 = 1
    | otherwise = fibonacci(x-1) + fibonacci(x-2)

-- returns the maximum value in a list
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "no elements in an empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- reproducing replicate function
replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x

-- reproducing take function
take' :: Int -> [a] -> [a]
take' n _ 
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- How to make a modified take functions that takes values starting at the end(tail) of a list?

-- reproducing reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- reproducing repeat
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- reproducing zip
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- reproducing elem
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) 
    | a == x = True
    | otherwise = a `elem'` xs

-- implementing QuickSort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerOrEqual = [y | y <- xs, y <= x]
        larger         = [y | y <- xs, y > x]
    in  quicksort smallerOrEqual ++ [x] ++ quicksort larger