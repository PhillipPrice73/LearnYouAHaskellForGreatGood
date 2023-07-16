module Arithmatic
( myAdd
, mySubtract
, myMultiply
, myDivide
) where

myAdd :: (Num a) => a -> a -> a
myAdd = (+)

mySubtract :: (Num a) => a -> a -> a
mySubtract = (-)

myMultiply :: (Num a) => a -> a -> a
myMultiply = (*)

myDivide :: (Fractional a) => a -> a -> a
--myDivide _ 0 = error "Dividing by 0"
myDivide x y = myMultiply x $ myInverse y

myInverse :: (Fractional a) => a -> a
myInverse x = 1 / x