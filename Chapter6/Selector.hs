{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingVia #-}
module Selector where

import Data.Kind

class Select (f :: Type -> Type) where
  empty :: f a
  pick :: f a -> f a -> f a

instance Select Maybe where
  empty = Nothing
  pick Nothing a = a
  pick a _ = a

instance Select [] where
  empty = []
  pick = (<>)

-- newtype MyMaybe a = MyMaybe (Maybe a) deriving Show
newtype MyMaybe a = MyMaybe (Maybe a)
  deriving Show
  deriving (Semigroup, Monoid) via (Sel Maybe a)

instance Semigroup (MyMaybe a) where
  (MyMaybe a) <> (MyMaybe b) = MyMaybe (pick a b)

instance Monoid (MyMaybe a) where
  mempty = MyMaybe empty

newtype Sel (f :: Type -> Type) (a :: Type) = Sel (f a)

instance (Select f) => Semigroup (Sel f a) where
  (Sel a) <> (Sel b) = Sel (pick a b)

instance (Select f) => Monoid (Sel f a) where
  mempty = Sel empty

