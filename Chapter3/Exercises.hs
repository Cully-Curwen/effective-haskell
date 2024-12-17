module Exercises where

-- addThree :: Int -> Int -> Int -> Int
-- has so many undefined possible definitions as there can be partialy applied undefined functions
-- i.e. addThree a = undefined
-- There is also they use of sub function, and combined functions with undefined
--
-- Data.Tuple.swap :: (a,b) -> (b,a) -- swaps fst and 2nd tuple elements
-- swap (x,y) = (y,x)
-- concat joins list of list into a single list
-- id a -> a return itself

mapApply :: [a -> b] -> [a] -> [b]
mapApply toApply =
  concatMap (\input -> map ($ input) toApply)

example :: [Int] -> String
example = map lookupLetter . mapApply  offsets
  where
    letters :: [Char]
    letters = ['a'..'z']

    lookupLetter :: Int -> Char
    lookupLetter n = letters !! n
    
    offsets :: [Int -> Int]
    offsets = [rot13, swap10, mixupVowels]
    
    rot13 :: Int -> Int
    rot13 n = (n + 13) `rem` 26
    
    swap10 :: Int -> Int
    swap10 n
      | n <= 10 = n + 10
      | n <= 20 = n - 10
      | otherwise = n
    
    mixupVowels :: Int -> Int
    mixupVowels n =
      case n of
      0 -> 8
      4 -> 14
      8 -> 20
      14 -> 0
      20 -> 4
      n' -> n'


