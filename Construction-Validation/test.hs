import Control.Monad (liftM)
import Natural

f :: Natural Integer -> Integer
f = (* 10) . unNatural

main = do
  putStr "Enter a Natural number: "
  n <- liftM read getLine
  putStrLn $ maybe "Supplied value is not Natural." (("Value * 10: "++).show.f) (mkNatural n)
