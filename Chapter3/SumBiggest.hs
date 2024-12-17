module SumBiggest where

import Data.List

sumBiggest :: [[Int]] -> String
sumBiggest allNums =
  let
    getBiggest :: [Int] -> [Int]
    getBiggest nums = undefined

    getSmallest :: [Int] -> [Int]
    getSmallest nums = undefined

    differences :: ([Int],[Int]) -> Int
    differences pairs = sum $ uncurry (zipWith (-)) pairs

    allBiggests :: [[Int]]
    allBiggests = map getBiggest allNums

    allSmallests :: [[Int]]
    allSmallests = map getSmallest allNums

    sizePairs :: [([Int],[Int])]
    sizePairs = zip allBiggests allSmallests

    differences' :: [String]
    differences' = map (show . differences) sizePairs
  in Data.List.intercalate "," differences'

