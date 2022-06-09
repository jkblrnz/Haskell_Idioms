module Collatz (collatzSequence, collatz, collatzCount) where

collatz :: Int -> Int
collatz 1 = 1
collatz n
  | even n    = n `div` 2
  | otherwise = n * 3 + 1

collatzSequence :: Int -> [Int]
collatzSequence n
  | n == 1     = [1]
  | n == (-2)  = [-2]
  | n == (-5)  = [-5]
  | n == (-17) = [-17]
  | otherwise  = n : collatzSequence (collatz n)

collatzCount :: Integer -> Maybe Integer
collatzCount n
  | n <= 0    = Nothing
  | n == 1    = Just 0
  | n == -2   = Just 0
  | n == -5   = Just 0
  | n == -17  = Just 0
  | even n    = succ <$> collatzCount (div n 2)
  | otherwise = succ <$> collatzCount (n * 3 + 1)
