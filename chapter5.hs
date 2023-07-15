{-
Chapter 5
Higher-Order Functions
-}

-- Functions that take functions as parameters and/or return functions as return values

{- 
Something like Int -> Int -> Int -> Int
is equivalent to Int -> (Int -> (Int -> Int)).
I.e. the first function takes an Int argument and returns a function
whose type is Int -> Int -> Int [or Int -> (Int -> Int)].
This happens recursively until we have the function who takes an Int argument and returns an Int.

This behavior of functions (only actually reading one parameter) is called currying (the function is a curried function).
-}

-- The following two functions are equivalent
compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

-- This takes advantage of the nature of curried functions
compareWithHundred' :: Int -> Ordering
compareWithHundred' = compare 100

-- Using sections to partially apply infix functions
-- this automatically puts the supplied argument on the side of the infix function that is currently missing an argument
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)
-- (/10) x --> x / 10

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])
-- (`elem` ['A'..'Z']) x --> x `elem` ['A'..'Z']

-- taking functions as an argument
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


-- reimplementing zipWith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- reimplementing flip
flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x


-- the map function sends operates on a list of values and outputs (in a list) the result of a applying a function to the values
-- remimplementation of map function from standard library
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x) : map f xs


-- reimplementation of filter function from standard library
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs

-- reimplementing QuickSort (compare with implementation in chapter4.hs)
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerOrEqual = filter (<= x) xs
        greaterThan = filter (> x) xs
    in quicksort smallerOrEqual ++ [x] ++ quicksort greaterThan

-- returns the largest number divisible by 3829 and smaller than 100000
largestDivisible :: Integer
largestDivisible = head (filter p [99999,99998..])
    where p x = x `mod` 3829 == 0

-- implementation of generating a Collatz sequence 
collatzSequence :: Integer -> [Integer]
collatzSequence 1 = [1]
collatzSequence x
    | x < 0 = []
    | even x = x : collatzSequence (x `div` 2)
    | odd x = x : collatzSequence (3*x + 1)

longchain :: Int
longchain =  length( filter isLong (map collatzSequence [1..100]))
    where isLong x = length x >= 15

-- using a lambda function to implement longchain
longchain' :: Int
longchain' = length ( filter (\x -> length x >= 15) (map collatzSequence [1..100]))

lambdaexample1 = zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
-- output: [153.0, 61.5, 31.0, 15.75, 6.6]
lambdaexample2 = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
-- output: [3, 8, 9, 8, 7]

sample1 = zipWith (flip (++)) ["love you", "love me"] ["i ", "you "]
-- output: ["i love you", "you love me"]

-- folds operate element-wise on lists
-- using left fold to implement sum function
sum' :: (Num a) => [a] -> a 
sum' = foldl (+) 0

-- using right fold to implement map function
newmap :: (a -> b) -> [a] -> [b]
newmap f xs = foldr (\x acc -> f x : acc) [] xs

-- using left fold to implement map function
newmap' :: (a -> b) -> [a] -> [b]
newmap' f xs = foldl (\acc x -> acc ++ [f x]) [] xs
-- ++ is much slower than :, so foldr is preferred when constructing lists
-- foldr also works on inifinte lists, whereas foldl does not

-- reimplementing elem with folding
elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = foldr (\y acc -> if y == x then True else acc) False xs 

-- calling a fold on an empty list just returns the provided accumulator value 

-- foldl1 and foldr1 using their respective starting element in the list as the initial accumulation value
-- As a result, these will cause runtime errors if called on an empty list, so beware

-- implementing maximum with foldl1
maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max


-- Reimplementations of various functions using folds
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

fancyreverse :: [a] -> [a]
fancyreverse = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

newfilter :: (a -> Bool) -> [a] -> [a]
newfilter p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- scans function like folds, but they output successive accumulator values as a list
-- One useful application is in monitoring the behavior of a function that is implemented using a fold
-- Outputs number of sqrts we need to sum together to exceed a value of 1000
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- left associative : a b c d e --> ((((a b) c) d) e)
-- right associative : a b c d e --> (a (b (c (d e))))

-- The following three definitinos are equivalent
functionApplicationOperationExample1 = sum (filter (> 10) (map (*2) [2..10]))
functionApplicationOperationExample2 = sum $ filter (> 10) (map (*2) [2..10])
functionApplicationOperationExample3 = sum $ filter (> 10) $ map (*2) [2..10]
-- output: 80 


functionCompositionExample1 = map (negate . abs) [5, -3, -6, 7, -3, 2, -19, 24]
-- output: [-5, -3, -6, -7, -3, -2, -19, -24]

functionCompositionExample2 = map (negate . sum . tail) [[1..5],[3..6],[1..7]]
-- output: [-14, -15, -27]

functionCompositionExample3 = sum . replicate 5 $ max 6.7 8.9
-- output: 44.5

-- the following two functions are equivalent
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]