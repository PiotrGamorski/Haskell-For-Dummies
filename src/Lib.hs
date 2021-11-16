{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import System.IO ()
import GHC.OldList (elemIndex)

someFunc :: IO ()
someFunc = putStrLn "Hey buddies we're really cool honest"

triple :: Int -> Int
triple x = x * 3

addMe :: Int -> Int
addMe a = (+) a 10

divMeByTwo :: Int -> Int
divMeByTwo a = div a 2

-- below there is a variable "three" of type Int.
-- It uses two built in functions to compute the result
three :: Int
three =
  let funcOne a = a + 2
      funcTwo = 12
   in funcOne funcTwo

posOrNeg :: (Ord a, Num a) => a -> [Char]
posOrNeg x =
  if x >= 0
    then "Positive"
    else "Negative"

posOrNeg' :: (Ord a, Num a) => a -> [Char]
posOrNeg' x
 | x >= 0 = "Positive"
 | otherwise = "Negative"

pow2 :: Int -> Int
pow2 n = pow2loop n 1 0

pow2loop :: Int -> Int -> Int -> Int
pow2loop n x i =
  if i < n
    then pow2loop n (x * 2) (i + 1)
    else x

pow2' :: (Ord t, Num p, Num t) => t -> p
pow2' n = pow2loop' n 1 0

pow2loop' :: (Ord t, Num p, Num t) => t -> p -> t -> p
pow2loop' n resInit i
 | i < n = pow2loop' n (resInit * 2) (i + 1)
 | otherwise = resInit

-- THEOREM: evry loop can be constructed with recursion.
-- How to execute the latter function in GHCI? Run: someLoop 3 (\a->a) 1 0
-- The latter function in not constructed well as it takes anonym. func in body 
someLoop :: (Ord t, Num a, Num t) => t -> (a -> a) -> t -> a -> a
someLoop n result i
 | i < n = someLoop n (\ result -> result * 2 + 1) (i + 1)
 | otherwise = result

-- the same achieved with where statement. Maybe above $ operator will be better instead of arrow func?
someLoop' :: (Ord t, Num p, Num t) => t -> p -> t -> p
someLoop' n input counter
  | counter < n = someLoop' n (loopLogic input) (counter + 1)
  | otherwise = input
  where loopLogic x = x * 2 + 1

-- To execute run for instance: anotherLoop 3 1 0 (\x->2*x+1)
-- or use the loopFunc below i.e., anotherLoop 3 1 0 loopFunc
anotherLoop :: (Ord t1, Num t1) => t1 -> t2 -> t1 -> (t2 -> t2) -> t2
anotherLoop n input counter loopFunc
 | counter < n = anotherLoop n (loopFunc input) (counter + 1) loopFunc
 | otherwise = input

loopFunc :: Num a => a -> a
loopFunc x = 2 * x + 1

newPow2 :: Int -> Int
newPow2 n =
  if n == 0
    then 1
    else 2 * newPow2 (n-1)

powNum :: (Eq p, Num p, Num t, Ord t) => p -> t -> p
powNum a n
 | a == 0 && n == 0 = 1
 | a == 0 && n > 0 = 0
 | a /= 0 && n == 0 = 1
 | a /= 0 && n >= 0 = a * powNum a (n-1)

-- the same function but using "Patern Matching"
pow2Pattern :: (Eq t, Num t, Num p) => t -> p
pow2Pattern 0 = 1
pow2Pattern n = 2 * pow2Pattern (n -1)

-- the same function but using "Guards"
pow2Guard :: (Eq t, Num t, Num p) => t -> p
pow2Guard n
  | n == 0 = 1
  | otherwise = 2 * pow2Guard (n -1)

-- ------------------LISTS IN HASKELL------------------
-- tail [1,2,3,4] = [2,3,4] and head [2,3,4] = 2. So, elem = 2
elem :: Integer
elem = head (tail [1,2,3,4])

doubleNums :: Num a => [a] -> [a]
doubleNums nums =
  if null nums
    then []
    else (2 * head nums) : doubleNums (tail nums)

doubleNums' :: Num a => [a] -> [a]
doubleNums' nums
 | null nums = []
 | otherwise = double' (head nums) : doubleNums' (tail nums)
 where double' x = 2 * x

-- the same result but using "Patern Matching"
doubleNums'' :: Num a => [a] -> [a]
doubleNums'' [] = []
doubleNums'' (x : xs) = double' x : doubleNums'' xs
 where double' num = 2 * num

-- the same function but using "Patern Matching" and map
double :: Num a => [a] -> [a]
double = map (2 *)

-- Haskell knows that there is a missing argument - a list of integers! This is a very short syntax
doubleWithMap :: [Integer] -> [Integer]
doubleWithMap = map (2 *)

modifyListElem :: Num a => [a] -> [a]
modifyListElem = map (\ x -> 2 * x + 1)

-- a little bit more complex examples

removeOdd :: Integral a => [a] -> [a]
removeOdd nums =
  if null nums
    then []
    else
      let firstNumber = head nums
       in if even firstNumber
            then firstNumber : removeOdd (tail nums)
            else removeOdd (tail nums)

-- the same function but using "Patern Matching"
removeOddPattern :: Integral a => [a] -> [a]
removeOddPattern [] = []
removeOddPattern (x : xs) =
  if even x
    then x : removeOddPattern xs
    else removeOddPattern xs

-- the same function but using "Guards"
removeOddGuard :: Integral a => [a] -> [a]
removeOddGuard [] = []
removeOddGuard (x : xs)
  | even x = x : removeOddGuard xs
  | otherwise = removeOddGuard xs

-- Another approach, this time with the use of Case
removeOddCase :: Integral a => [a] -> [a]
removeOddCase nums = case nums of
  [] -> []
  (x : xs) ->
    if even x
      then x : removeOddCase xs
      else removeOddCase xs

-- This is the easiest way to achive removing odd numbers from a list
removeOddFilter :: [Integer] -> [Integer]
removeOddFilter = filter even

-- We can change a diffrent conditions using filter
filterList :: [Integer] -> [Integer]
filterList = filter (\ a -> even a && a > 2 && a < 7)

anyEven :: Integral a => [a] -> Bool
anyEven nums = case removeOddGuard nums of
  [] -> False
  (x : xs) -> True

-- How to achieve the same with filter?
isEven :: Integer -> Bool
isEven = even

-- this one is overcomplicated
onlyEven :: [Integer] -> [Integer]
onlyEven = filter isEven

isGreaterThan :: (Ord a, Num a) => a -> Bool
isGreaterThan num
  | num > 5 = True
  | otherwise = False

evenOrOdd :: Integral p => p -> p
evenOrOdd num
  | even num = 2 * num
  | otherwise = num

increaseEveryEven :: Integral b => [b] -> [b]
increaseEveryEven = map evenOrOdd

-- the latter syntax is the shortcut for "\x -> elemIndex x nums"
-- Haskell knows that map takes an argument from a list and passes it immediately to elemIndex.
getAllIndexesOfList :: Eq a => [a] -> [Maybe Int]
getAllIndexesOfList elems = map (`elemIndex` elems) elems

a :: Maybe Int
a = elemIndex 4 [1, 2, 3, 4, 5, 6, 7, 8, 9]
-- including fromMaybe(-1) will get rid off "Just" and return the value.
b :: Int
b = fromMaybe (-1) $ elemIndex 4 [1,2,3,4,5,6,7,8,9]

-- fromMaybe (-1): "Just Value" -> Value 
mupliplyEvenListElement :: (Eq p, Num p) => p -> [p] -> p
mupliplyEvenListElement a list
  | even (fromMaybe (-1) $ elemIndex a list) = 2 * a
  | otherwise = a

multiplyEverySecond :: (Eq b, Num b) => [b] -> [b]
multiplyEverySecond nums = map (`mupliplyEvenListElement` nums) nums

multiplyEverySecond' :: (Eq b, Num b) => [b] -> [b]
multiplyEverySecond' nums = map (`multiplyElemByEvenIndex` nums) nums
 where multiplyElemByEvenIndex a list
        | even (fromMaybe (-1) $ elemIndex a list) = 2 * a
        | otherwise = a


listModify :: (Eq b, Num b) => [b] -> [b]
listModify [] = []
listModify nums = map (`pickAndModifyElem` nums) nums
 where pickAndModifyElem x list
        | fromMaybe (-1) (elemIndex x list) > 3 = 10 * x
        | otherwise = x

listModify' :: (Num b, Eq b) => [b] -> [b]
listModify' [] = []
listModify' nums = map(`pickAndModifyElem` nums) nums
 where pickAndModifyElem x list =
        let {index = fromMaybe (-1) (elemIndex x list)}
        in
         if index > 3 then 5 * x
         else if index == 3 then 10 * x + 3
         else if index < 3 && index >= 2 then 3 * x
         else x

-- With "case of" we cannot use inequality conditions. How to use the guards?
listModify'' :: (Num b, Eq b) => [b] -> [b]
listModify'' [] = []
listModify'' nums = map(`pickAndModifyElem` nums) nums
 where pickAndModifyElem x list =
        let {index = fromMaybe (-1) (elemIndex x list)}
        in
          case index of
            3 -> 3 * x
            _ -> x

newListModify :: (Num b, Eq b) => [b] -> [b]
newListModify [] = []
newListModify nums = map(`pickAndModifyElem` nums) nums
 where pickAndModifyElem x list 
          | index > 3 = 5 * x
          | index == 3 = 10 * x +3
          | index < 3 && index >= 2 = 3 * x
          | index == 1 = x + 47 
          | otherwise = -x
          where index = fromMaybe (-1) (elemIndex x list)

greaterThan :: [Integer] -> [Integer]
greaterThan = filter (> 5)

countEven :: Integral a => [a] -> Int
countEven nums =
  let evenNums = removeOddGuard nums
   in length evenNums

createList :: (Eq a, Num a) => a -> [a]
createList n
  | n == 0 = []
  | otherwise = n : createList (n -1)

createListWithLoop :: (Ord t, Num t) => t -> [t]
createListWithLoop n = loopList n [] 0

loopList :: (Ord t, Num t) => t -> [t] -> t -> [t]
loopList n x i
  | i < n = loopList n (n - i : x) (i + 1)
  | otherwise = x

customFunction :: (Eq p, Fractional p) => p -> p
customFunction 0 = 0
customFunction 1 = 10
customFunction x = 1 / x

pass :: t1 -> (t1 -> t2) -> t2
pass x f = f x

pass3 :: (Integer -> t2) -> t2
pass3 = pass 3

add1 :: Num a => a -> a
add1 x = x + 1

-- some examples how foldl and foldr work
showPlus :: Show a => String -> a -> String
showPlus s x = "(" ++ s ++ "+" ++ show x ++ ")"

seeFoldl :: String
seeFoldl = foldl showPlus "0" [1, 2, 3, 4]

showPlus' :: Show a => a -> String -> String
showPlus' x s = "(" ++ show x ++ "+" ++ s ++ ")"

seeFoldr :: String
seeFoldr = foldr showPlus' "0" [1, 2, 3, 4]

seeFoldlWithNumbers :: Integer
seeFoldlWithNumbers = foldl (-) 0 [1, 2, 3]

-- (((0 -1) -2) -3) = -1 -2 -3 = -6
seeFoldrWithNumbers :: Integer
seeFoldrWithNumbers = foldr (-) 0 [1, 2, 3]

-- (1-(2-(3 - 0))) = 1 - 2 + 3 = 2

x :: String
x = show (read "123" :: Int)

lengthOfAnyArray :: [a] -> Int
lengthOfAnyArray [] = 0
lengthOfAnyArray (x : xs) = length xs + 1

-- How to understand the function definition by  Num p => [p] -> p ?
-- Types such as Int, Integer, Float, and Double come under this Type class Num
-- for any numeric (class) type p, the function takes list of values of type p as argument
--  and returns value of type p
theSum :: Num p => [p] -> p
theSum [] = 0
theSum (x : xs) = x + theSum xs

-- for any foldable (class) type t and numeric (class) type b, the function takes argument as a value of type b
-- (b stands for 0 in this example) and returns value of type b.
-- since "foldl" or "foldr" take array and function as arguments, this info is incuded in "Foldable t"!
theSumFold :: (Foldable t, Num b) => t b -> b
theSumFold = foldl (+) 0
-- (...(((0 + x1) + x2) + x3) + ... xn)

sumVector :: (Foldable t, Num a) => t a -> a
sumVector = sum

doPrinting :: IO ()
doPrinting = print (2 :: Float)