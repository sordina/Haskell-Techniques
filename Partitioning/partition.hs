-- Partitioning problem as described in the article:
-- http://www.americanscientist.org/issues/id.3278,y.0,no.,content.true,page.1,css.print/issue.aspx

import Data.List
import Data.Function (on)

-- Common data

input = [771, 121, 281, 854, 885, 734, 486, 1003, 83, 62]

sortedSL = sort input
sortedLS = sortBy (flip compare) input

-- Simple (Baseball style)

partitioned = divide sortedLS

divide :: [a] -> ([a],[a])

divide (x:y:xs) = let (l,r) = divide xs in (x:l,y:r)
divide [x]      = ([x],[])
divide []       = ([],[])

answer = diffSum partitioned

-- Glutonous

glutton :: Int -> Int -> [Int] -> ([Int],[Int])
glutton _ _ [] = ([],[])
glutton l r (h:t)
  | l > r     = let (nl,nr) = glutton l (r+h) t in (nl,h:nr)
  | otherwise = let (nl,nr) = glutton (l+h) r t in (h:nl,nr)

gluttonSL = glutton 0 0 sortedSL
gluttonLS = glutton 0 0 sortedLS

-- Greedy

greedy :: Int -> Int -> [Int] -> ([Int],[Int])
greedy _ _ [] = ([],[])
greedy l r remaining
  | l > r     = let (nl,nr) = greedy l (r+hb) tb in (nl,hb:nr)
  | otherwise = let (nl,nr) = greedy (l+hb) r tb in (hb:nl,nr)
  where
    hb = head best
    tb = tail best
    best = sortBy (compare `on` (abs . (diff -))) remaining
    diff = abs (l - r)

diffSum (l,r) = abs (sum l - sum r)

greedy1 = greedy 0 0 sortedLS
greedy2 = let (l:r:xs) = sortedLS in greedy l r xs


-- Answers

main = do
  print answer
  print $ diffSum greedy1
  print $ diffSum greedy2
  print $ diffSum gluttonSL
  print $ diffSum gluttonLS -- WINNER!!!!
