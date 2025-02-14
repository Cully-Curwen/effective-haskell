module HaskellBook.Examples.SortedList
  ( SortedList (getSorted)
  , makeSortedList
  , minimum
  ) where

import Data.List (sort)
import Prelude hiding (minimum)

data SortedList = SortedList { getSorted :: [Int] }

makeSortedList :: [Int] -> Maybe SortedList
makeSortedList [] = Nothing
makeSortedList numbers = Just $ SortedList (sort numbers)

minimum :: SortedList -> Int
minimum (SortedList numbers) = head numbers
