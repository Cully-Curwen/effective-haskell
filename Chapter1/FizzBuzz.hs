module FizzBuzz where

fizzBuzzFor number
  | 0 == number `rem` 15 = "fizzbuzz"
  | 0 == number `rem` 5  = "buzz"
  | 0 == number `rem` 3  = "fizz"
  | otherwise = show number


