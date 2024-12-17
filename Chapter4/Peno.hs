module Peano where

data Peano = Z | S Peano

toPeano :: Int -> Peano
toPeano 0 = Z
toPeano n = S (toPeano $ n - 1)

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S p) = succ (fromPeano p)

eqPeano :: Peano -> Peano -> Bool
eqPeano p p' =
  case (p,p') of
    (Z,Z) -> True
    (S n, S n') -> eqPeano n n'
    _ -> False

addPeano :: Peano -> Peano -> Peano
addPeano Z b = b
addPeano (S a) b = addPeano a (S b)


data List a = Empty | Cons a (List a)

toList :: [a] -> List a
-- toList [] = Empty
-- toList (x:xs) = Cons x (toList xs)
toList = foldr Cons Empty

fromList :: List a -> [a]
-- fromList Empty = []
-- fromList (Cons x xs) = x : fromList xs
fromList = listFoldr (:) []

listFoldr :: (a -> b -> b) -> b -> List a -> b
listFoldr _ ys Empty = ys
listFoldr f ys (Cons x xs) = f x (listFoldr f ys xs)

listFoldl :: (b -> a -> b) -> b -> List a -> b
listFoldl _ y Empty = y
listFoldl f y (Cons x xs) = listFoldl f (f y x) xs

listHead :: List a -> Maybe a
listHead Empty = Nothing
listHead (Cons x _) = Just x

listTail :: List a -> List a
listTail Empty = Empty
listTail (Cons _ xs) = xs

listReverse :: List a -> List a
listReverse Empty = Empty
listReverse xs = listFoldl (flip Cons) Empty xs

listMap :: (a -> b) -> List a -> List b
listMap _ Empty = Empty
listMap f (Cons x xs) = Cons (f x) (listMap f xs)
