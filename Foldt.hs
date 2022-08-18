-- mergesort to demonstrate expressiveness of foldt
-- sieve of Eratosthenes to demonstrate expressiveness of foldi
module Foldt where
import Data.List

-- tree-like folds
pairs           :: (a -> a -> a) -> [a] -> [a]
pairs f (x:y:z) = f x y : pairs f z
pairs _ z       = z

foldt         :: (a -> a -> a) -> a -> [a] -> a
foldt _ z []  = z
foldt f z [x] = f x z
foldt f z xs  = foldt f z (pairs f xs)

foldi            :: (a -> a -> a) -> a -> [a] -> a
foldi f z []     = z
foldi f z (x:xs) = f x (foldi f z (pairs f xs))

-- "(((((1+2)+(3+4))+((5+6)+(7+8)))+(((9+10)+(11+12))+13))+0)"
foldtExample :: [Char]
foldtExample = foldt (\x y -> concat ["(",x,"+", y,")"]) "0" (map show [1..13])

-- "(1+((2+3)+(((4+5)+(6+7))+((((8+9)+(10+11))+(12+13))+0))))"
foldiExample :: [Char]
foldiExample = foldi (\x y -> concat ["(",x,"+", y,")"]) "0" (map show [1..13])

-- "(1+(2+(3+(4+(5+(6+(7+(8+(9+(10+(11+(12+(13+0)))))))))))))"
foldrExample :: [Char]
foldrExample = foldr (\x y -> concat ["(",x,"+", y,")"]) "0" (map show [1..13])

-- "(((((((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)+10)+11)+12)+13)"
foldlExample :: [Char]
foldlExample = foldl (\x y -> concat ["(",x,"+", y,")"]) "0" (map show [1..13])

-- mergesort
merge                     :: Ord a => [a] -> [a] -> [a]
merge [] ys               = ys
merge xs []               = xs
merge xs@(x:xt) ys@(y:yt) | x <= y    = x : merge xt ys
                          | otherwise = y : merge xs yt

sort    :: Ord a => [a] -> [a]
sort xs = foldt merge [] [[x] | x <- xs]

-- sieve of Eratosthenes
primes :: (Integral a) => [a]
primes = 2 : 3 : ([5,7..] `intersect` foldi (\(x:xs) -> (x:) . union xs) [] [[p*p, p*p+2*p..] | p <- tail primes])
