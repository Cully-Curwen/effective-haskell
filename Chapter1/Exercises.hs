module Exercise where

factorial 0 = 0
factorial n = n + factorial (n - 1)

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

uncurry' f (a, b) = f a b

curry' f a b = f (a, b)
