import Control.Monad
import Data.Maybe
import Debug.Trace

instance Num n => Num (Maybe n)
  where
    (+)         = liftM2 (+)
    (*)         = liftM2 (*)
    abs         = liftM abs
    signum      = liftM signum
    fromInteger = Just . fromInteger

instance Fractional (Maybe Rational)
  where
    fromRational = Just
    recip = (>>= \n -> if n == 0 then Nothing else Just (1/n))

newtype Overflowable n = O {stagnant :: Maybe n}
  deriving (Eq, Show, Ord, Monad) -- Haskell extensions allow generallized diriving of Monad and Num

instance Bounded n => Bounded (Overflowable n)
  where
    minBound = O $ Just minBound
    maxBound = O $ Just maxBound

instance (Ord n, Bounded n, Num n) => Num (Overflowable n)
  where
    a + b         = if (a > maxBound - b) || (a < minBound + b) -- TODO: think more about the overflow cases
                     then O Nothing
                     else liftM2 (+) a b

    (*)           = liftM2 (*)
    fromInteger   = O . Just . fromInteger
    abs           = liftM abs
    signum        = liftM signum


-- Show off the new ideas

a = Nothing
b = Just 2
c = Just 5

d = b / c :: Maybe Rational
e = Just 0
f = d * c / e

g = 10^5 :: Overflowable Int
h = take 20 $ iterate (+g) g

main = mapM_ print [ a, b, c, d, a + b, b * c, c / d, f ]
