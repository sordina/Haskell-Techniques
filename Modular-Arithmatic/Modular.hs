module Modular

  (Modular(),
   mkModular,
   runModular)

where

import Test.QuickCheck

data Integral a => Modular a = Modular {modulus :: a, runModular :: a}
  deriving (Eq,Show,Ord)

instance Integral a => Num (Modular a)
  where
    a + b = mkModular nmod (runModular a + runModular b) where nmod = gcdMod a b
    a * b = mkModular nmod (runModular a * runModular b) where nmod = gcdMod a b
    abs a = a
    signum a = 1
    fromInteger a = mkModular (fromIntegral a) 0

gcdMod :: Integral a => (Modular a) -> (Modular a) -> a
gcdMod a b = gcd (modulus a) (modulus b)

mkModular :: Integral a => a -> a -> Modular a
mkModular modulus = Modular modulus . (`mod` modulus)

prop_modular = b * c `mod` a == runModular dM
  where
    a = 11
    b = 13
    c = 111
    bM = mkModular a b
    cM = mkModular a c
    dM = bM * cM
