import Control.Monad

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
  deriving (Eq, Show, Ord)

instance Bounded (Overflowable n)
  where
    minBound = undefined
    maxBound = undefined

instance Monad (Overflowable)
  where
    -- (Monad m) => m a -> (a -> m b) -> m b
    --(O Nothing)  >>= f = O Nothing
    --(O (Just a)) >>= f = O (Just (f a))
    (>>=) = undefined
    return n = O (Just n)
    --return = undefined

instance (Num n, Bounded n) => Num (Overflowable n)
  where
    (+)         = liftM2 (+)
    (*)         = liftM2 (*)
    abs         = liftM abs
    signum      = liftM signum
    fromInteger = O . Just . fromInteger

a = Nothing
b = Just 2
c = Just 5

d :: Maybe Rational
d = b / c
e = Just 0
f = d * c / e

main = mapM_ print [ a, b, c, d, a + b, b * c, c / d, f ]
