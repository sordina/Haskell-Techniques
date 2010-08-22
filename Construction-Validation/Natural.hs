module Natural (Natural(unNatural), mkNatural) where

newtype Integral i => Natural i = Natural {unNatural :: i} deriving (Show, Eq, Ord)

mkNatural :: Integral i => i -> Maybe (Natural i)
mkNatural n
  | n >= 0    = Just $ Natural n
  | otherwise = Nothing
