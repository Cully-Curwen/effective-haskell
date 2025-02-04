import Text.Read (readMaybe)
import Data.Char

-- Return half of a value if it's even, otherwise Nothing
half :: Int -> Maybe Int
half num =
  if even num
  then Just (num `div` 2)
  else Nothing

-- Takes a boundry. Returns Just the value if it's within the range,
-- and Nothing otherwise
bound :: (Int, Int) -> Int -> Maybe Int
bound (min, max) num =
  if (num >= min) && (num <= max)
  then Just num
  else Nothing

data Outlaw a = Outlaw Int a deriving (Eq, Show)

instance Functor Outlaw where
  fmap f (Outlaw cnt val) = Outlaw (cnt + 1) (f val)

bang = (<> "!")
upcase = map Data.Char.toUpper
billyTheKid = Outlaw 0 "bank robber"

testIdentity = fmap id billyTheKid == id billyTheKid

testComposition =
  fmap (bang . upcase) billyTheKid == (fmap bang . fmap upcase $ billyTheKid)

instance Monad Outlaw where
  return summary = Outlaw 0 summary
  (Outlaw cnt a) >>= f =
    let (Outlaw cnt' v) = f a
    in Outlaw (cnt + cnt' + 1) v

stoleAHorse :: String -> Outlaw String
stoleAHorse = return . (<> " and horse robber")

testLeftIdentity =
  (return "robbed a bank" >>= stoleAHorse) == stoleAHorse "robbed a bank"

testRightIdentity =
  (billyTheKid >>= return) == billyTheKid

