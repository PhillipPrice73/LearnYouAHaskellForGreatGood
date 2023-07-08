{-
Syntax in Functions 
-}

-- Basic pattern matching
-- Useful for checking if values passed to the function are constructed in a specific way
sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe x = "Not between 1 and 3"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

greeting :: Char -> String
greeting 'a' = "Albert!"
greeting 'b' = "Bob!"
greeting 'c' = "Charles!"
-- example of non-exhaustive pattern matching.  
-- Include a generic catch-all pattern at end (not the beginning, as Haskell checks cases in the order their coded)

greeting' :: Char -> String
greeting' x = "Hello Anonymous!"
greeting' 'a' = "Albert"
greeting' 'b' = "Bob"
greeting' 'c' = "Charles"
-- this will always return "Hello Anonymous!"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list."
head'(x:_) = x
-- binding an item to several variables (_ is considered a variable) requires wrapping the item in parentheses

-- See Ch.2 for info on class types and class constraints
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element, " ++ show x
tell (x:y:[]) = "The list has two elements, " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list has many elements, the first two being " ++ show x ++ " and " ++ show y

-- As-patterns
firstLetter :: String -> String
firstLetter "" = "Empty string"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
-- as-patterns basically provide an option for keeping the original list together.
-- otherwise, we would need to have the following:
-- firstLetter (x:xs) = "The first letter of " ++ [x:xs] ++ " is " ++ [x]


-- Guards
-- Useful for checking properties of values passed into functions
bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "Undereweight"
    | bmi <= 25.0 = "Average"
    | bmi <= 30.0 = "Overweight"
    | otherwise = "Obese"
-- Guard clauses must be indented by at least one space and are checked sequentially, just like pattern matching

bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | weight / height ^ 2 <= 18.5 = "Undereweight"
    | weight / height ^ 2 <= 25.0 = "Average"
    | weight / height ^ 2 <= 30.0 = "Overweight"
    | otherwise -> "Obese"

bmiTell2 :: Double -> Double -> String
bmiTell2 weight height
    | bmi <= underweight = "Undereweight"
    | bmi <= average = "Average"
    | bmi <= overweight = "Overweight"
    | otherwise = "Obese"
    where bmi = weight / height ^ 2
          underweight = 18.5
          average = 25.0
          overweight = 30.0
-- when using where, variable names need to all be aligned in a single column so Haskell groups them together

-- where bindings are only in the scope of the 'closest' function pattern
-- the following won't work
greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name
    where niceGreeting = "Nice to see you"
          badGreeting = "Oh, it's you"

-- the following will work
badGreeting :: String
badGreeting = "Oh! Pfft. It's you."

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you,"

greet' :: String -> String
greet' "Juan" = niceGreeting ++ " Juan"
greet' "Fernando" = niceGreeting ++ " Fernando"
greet' name = badGreeting ++ " " ++ name

-- pattern matching with where
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- Let expressions
-- let <bindings> in <expression
cylinderSA :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]


{- Case expressions -}
-- The following two functions are equivalent, with the second function using a case expression
myHead :: [a] -> a
myHead [] = error "Empty list"
myHead (x:_) = x

myHead' :: [a] -> a
myHead' xs = case xs of [] -> error "Empty list"
                        (x:_) -> x

-- The following two functions are equivalent
describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of  [] -> "empty."
                                                [x] -> "a singleton list."
                                                xs -> "a longer list."                                                

describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
    where   what [] = "empty."
            what [x] = "a singleton list."
            what xs = "a longer list."

