{-
Chapter 6
Modules
-}

-- import ModuleName

-- For only using specific functions:
-- import ModuleName (function1, function2,...)

-- For importing a module while excluding specified functions:
-- import ModuleName hiding (function1, function2, ...)

-- Can rename imported modules as in Python
-- import ModuleName as M

-- import qualified ModuleName
-- automatically excludes any functions that would cause name clashes with currently loaded functions
-- Can still be called with something like ModuleName.function

-- imports must be done before defining any functions, so usually done at the start of the file

import Data.List (words, group, sort, tails, isPrefixOf, any, find)
import Data.Char (ord, chr, digitToInt)
import Arithmatic

moduleFunctionExample1 = words "hey these are the words in this sentence"
-- output: ["hey", 'these', 'are', 'the','words','in','this','sentence']
-- words groups adjacent punctuation with the word.  
-- The groups are also case sensitive

moduleFunctionExample2 = group [1,1,1,2,2,2,3,3,3,4,4,4,5,5,5]
-- output: [[1,1,1],[2,2,2],[3,3,3],[4,4,4],[5,5,5]]

moduleFunctionExample3 = sort [5,4,3,2,1]
-- output: [1,2,3,4,5]
moduleFunctionExample4 = sort ["clang", "bang", "aang"]
-- output: ["aang", "bang", "clang"]

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words
{- How to improve this to ignore punctuation and different capitalizations? -}

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

-- implementing Caesar Cipher
encodeCaesar :: Int -> String -> String
encodeCaesar offset message = map (\c -> chr $ ord c + offset) message
-- equivalent to 
-- encodeCaesar = chr . ( + offset) . ord
{- I think it makes more sense to send the message first, offset second.  That way an infix call would be
message encodeCaesar offset.  How to rewrite this function (and the composition equivalent) to handle this? -}

decodeCaesar :: Int -> String -> String
decodeCaesar shift message = encodeCaesar (negate shift) message
{- How to rework the Caesar Cipher functions to stay within the alphabet (not pulling in symbols like -, #, !, etc.)? -}

-- Data.List.foldl' prevents stack overflows when using left folds with very large list sizes by not being lazy
-- similar version exists for foldl1 (foldl1')

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show 
-- show to turn Ints into a list of digits

-- Maybe keyword is used for return values to indicate that the value returned will be a list of either 0 or 1 elements
firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

firstTo :: Int -> Maybe Int
firstTo sum = find (\x -> digitSum x == sum) [1..]

-- Association Lists / Dictionaries / Maps
-- Look in Data.Map for standard implementations of these types, + associated utility functions

findKeyV1 :: (Eq k) => k -> [(k, v)] -> v
findKeyV1 key xs = snd . head . filter (\(k,v) -> key == k) $ xs
-- Bug is hidden in this implementation

findKeyV2 :: (Eq k) => k -> [(k, v)] -> Maybe v
findKeyV2 key [] = Nothing
findKeyV2 key ((k,v):xs)
    | key == k = Just v -- why Just v?  why not v?
    | otherwise = findKeyV2 key xs

findKeyV3 :: (Eq k) => k -> [(k, v)] -> Maybe v
findKeyV3 key xs = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing xs

moduleExample1 = myAdd 4 5
moduleExample2 = mySubtract 32.1 18.7
moduleExample3 = myMultiply 3 (-4)
moduleExample4 = myDivide 18 6
moduleExample5 = myDivide 12 0