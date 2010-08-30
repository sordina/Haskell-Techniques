import Test.QuickCheck
import Data.Char

-- Simple types

data Letter = A | B | C deriving Show

instance Arbitrary Letter
  where
    arbitrary = elements [B,C]
    coarbitrary = undefined

prop_letter :: Letter -> Bool
prop_letter B = True
prop_letter C = True
prop_letter _ = False

-- Types with existing Arbitrary instances

newtype CertainCharacter = CertainCharacter Char deriving Show

instance Arbitrary CertainCharacter
  where
    arbitrary = elements $ map CertainCharacter ['a'..'q']
    coarbitrary = undefined

prop_certainCharacters :: CertainCharacter -> Bool
prop_certainCharacters (CertainCharacter c) = isLower c

-- Cutting out the middleman

myRange :: Gen Char
myRange = elements (['A'..'Z'] ++ ['a' .. 'z'] ++ " ~!@#$%^&*()")

prop_simpleChars = forAll myRange $ \c -> chr (ord c) == c
