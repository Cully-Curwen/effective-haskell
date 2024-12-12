module Main where

printSmallNumber num =
  let msg = if num < 10
            then show num
            else "the number is too big!"
  in print msg

main = printSmallNumber 3
