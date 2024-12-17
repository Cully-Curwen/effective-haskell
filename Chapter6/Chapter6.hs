{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Chapter6 where

adheresToReadShowContract :: forall a. (Read a, Show a) => a -> Bool
adheresToReadShowContract val =
  let a = show . read @a . show $ val
      b = show val
  in a == b

-- newtype MyEither a b = MyEither (Either a b)
newtype MyEither a b = MyEither { getEither :: Either a b }


