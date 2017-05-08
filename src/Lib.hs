module Lib where

hello :: IO ()
hello = putStrLn "Hello, world!"

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- 1
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast [x,y] = x
myButLast (x:xs) = myButLast xs

-- 3
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) k = elementAt xs (k-1)

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 5
myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [x] = True
isPalindrome' xs = (head xs) == (last xs) && (isPalindrome' (init (tail xs)))

-- 7
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List x) = concatMap myFlatten x

--8
