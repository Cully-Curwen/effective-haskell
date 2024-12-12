module InfiniteFind where

findFirst predicate =
  foldr findHelper []
  where
    findHelper listElement maybeFound
      | predicate listElement = [listElement]
      | otherwise = maybeFound


