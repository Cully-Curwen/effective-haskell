module Exercises where

reversel xs = foldl (\ acc x -> x:acc) [] xs

reverser xs = foldr (\ x -> (<> [x])) [] xs

zipWithlc f xs ys = [f (xs !! i) (ys !! i) | i <- [0..(length xs)], i < length ys]

zipWithnc f [] _  = []
zipWithnc f _ []  = []
zipWithnc f (x:xs) (y:ys) = f x y : zipWithnc f xs ys

concatMapl f xs = foldl (\ acc x -> acc <> f x) [] xs

concatMapr f = foldr ((<>) . f) []

-- \f g -> foldr g 0 . map f
-- \f g -> foldr (g . f) 0
-- The above should end up beig equivalnt
--
-- \f g -> foldl g 0 . map f
-- \f g -> foldl (g . f) 0
-- The results may differ as in 2nd f is applied to the accumalator


-- foldr step through applying fn to n ellement reccursevely calling the rest as a thunk
-- foldl step through applying fn to n ellement to the accumilator, there is no thunk
