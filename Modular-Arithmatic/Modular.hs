module Modular (

  Modular(),
  mkModular,
  runModular
)

where

import Test.QuickCheck
import Data.Ratio
import Data.List (foldl')
import Debug.Trace

data Integral a => Modular a = Modular {modulus :: a, runModular :: a}
  deriving (Eq,Show,Ord)

instance Integral a => Num (Modular a)
  where
    a + b = mkModular nmod (runModular a + runModular b) where nmod = gcdMod a b
    a * b = mkModular nmod (runModular a * runModular b) where nmod = gcdMod a b
    abs a = a
    signum a = 1
    fromInteger a = mkModular (fromIntegral a) 0

gcdMod :: Integral a => Modular a -> Modular a -> a
gcdMod a b
  | d == 1    = lcm ma mb
  | otherwise = d
  where
    ma = modulus a
    mb = modulus b
    d = gcd ma mb

mkModular :: Integral a => a -> a -> Modular a
mkModular modulus = Modular modulus . (`mod` modulus)

modularPow2 :: Integral a => a -> a -> a -> a
modularPow2 b e m = foldl' f 1 exps
  where
    f a b = (a * b) `mod` m
    exps  = takeWhile (>0) $ iterate (`div` 2) e

prop_modularPow x = x > 1 ==> trace (show x ++ " - " ++ show a ++ " - " ++ show b) a == b
  where
    a = modularPow x 123 7
    b = (x ^ 123 `mod` 7)

prop_modularPow2 x = x > 1 ==> trace (show x ++ " - " ++ show a ++ " - " ++ show b) a == b
  where
    a = modularPow2 x 123 7
    b = (x ^ 123 `mod` 7)

square n = n * n

modularPow :: Integral a => a -> a -> a -> a
modularPow base exponent modulus = runModular $ foldl' f one exponents
  where
    one       = mkModular modulus 1
    mbase     = mkModular base modulus
    f a b     = a * b
    exponents = zipWith (*) bases $ map (mkModular modulus) $ takeWhile (>0) cuts
    mcuts     = map g cuts
    cuts      = iterate (`div` 2) exponent
    bases     = iterate square mbase
    g x       = if even x then Nothing else Just x

{--- Should be taken care of with the Real and Integral instances
 -
function modular_pow(base, exponent, modulus)
    result := 1
    while exponent > 0
        if (exponent & 1) equals 1:
           result = (result * base) mod modulus
        exponent := exponent >> 1
        base = (base * base) mod modulus
    return result
-}

{-
instance Integral a => Real (Modular a)
  where
    toRational a = fromIntegral (runModular a) % 1

instance (Integral a, Real a) => Enum (Modular a)
  where
    toEnum = fromInteger . fromIntegral
    fromEnum = fromIntegral . runModular

instance (Integral a, Real a) => Integral (Modular a)
  where
    toInteger = fromIntegral . runModular
    quotRem a b = (Modular nmod n, Modular nmod d)
      where
        nmod  = gcdMod a b
        (n,d) = runModular a `divMod` runModular b
-}

-- Properties

prop_modular = b * c `mod` a == runModular dM
  where
    a = 11
    b = 13
    c = 111
    bM = mkModular a b
    cM = mkModular a c
    dM = bM * cM
