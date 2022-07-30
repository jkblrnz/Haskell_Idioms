-- mergesort to demonstrate expressiveness of foldt
module Foldt where

pairs :: (a -> a -> a) -> [a] -> [a]
pairs f (x:y:z) = f x y : pairs f z
pairs _ z       = z

foldt :: (a -> a -> a) -> a -> [a] -> a
foldt _ z []  = z
foldt f z [x] = f x z
foldt f z xs  = foldt f z (pairs f xs)

-- mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
-- mergeBy cmp = merge where
--   merge [] ys = ys
--   merge xs [] = xs
--   merge (x:xs) (y:ys)
--     = case cmp x y of
--       GT -> y : merge (x:xs) ys
--       _  -> x : merge xs (y:ys)

-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- sortBy cmp = foldt (mergeBy cmp) [] . map (: [])

-- sort :: Ord a => [a] -> [a]
-- sort = sortBy compare

merge [] ys = ys
merge xs [] = xs
merge xs@(x:xt) ys@(y:yt)
  | x <= y = x : merge xt ys
  | otherwise = y: merge xs yt

mergesort xs = foldt merge [] [[x] | x <- xs]
