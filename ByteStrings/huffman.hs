import Data.Function
import Data.List
import Data.Maybe
import Data.Char
import GHC.Word
import qualified Data.ByteString as B

data Tree a = Leaf a | Tree (Tree a) (Tree a)
 deriving Show

type F = Tree (Char, Double)

bs = foldr B.cons B.empty . map (fromInteger . fromIntegral) . bytes

bytes = map num . chunk . encodeMessage

num = foldl1 (\x y -> x*2 + y)

chunk :: [a] -> [[a]]
chunk l@(_:_:_:_:_:_:_:_:_) = take 8 l : chunk (drop 8 l)
chunk l                     = [l]

encode l = encodeMessage l ++ replicate (len `mod` 8) 0
 where len = length l

encodeMessage :: String -> [Int]
encodeMessage = concat . mapMaybe (flip lookup codes) . map toLower

codes = sortBy (compare `on` (length.snd)) $ path reduction

path :: F -> [(Char,[Int])]
path (Leaf (c,_)) = [(c,[])]
path (Tree l r)   = map (f 0) pl ++ map (f 1) pr
 where
   f b (c,s) = (c,b:s)
   pl = path l
   pr = path r

reduction = reduce trees

reduce :: [F] -> F
reduce []  = error "Need at least one item in the list."
reduce [t] = t
reduce l   = reduce $ Tree a b : xs
 where (a:b:xs) = sortTrees l

sortTrees :: [F] -> [F]
sortTrees = sortBy (compare `on` value)

value :: F -> Double
value (Leaf (_,n)) = n
value (Tree l r)   = value l + value r

trees = map Leaf freqs

freqs = [
 ('a',8.167),
 ('b',1.492),
 ('c',2.782),
 ('d',4.253),
 ('e',12.702),
 ('f',2.228),
 ('g',2.015),
 ('h',6.094),
 ('i',6.966),
 ('j',0.153),
 ('k',0.772),
 ('l',4.025),
 ('m',2.406),
 ('n',6.749),
 ('o',7.507),
 ('p',1.929),
 ('q',0.095),
 ('r',5.987),
 ('s',6.327),
 ('t',9.056),
 ('u',2.758),
 ('v',0.978),
 ('w',2.360),
 ('x',0.150),
 ('y',1.974),
 ('z',0.074)]
