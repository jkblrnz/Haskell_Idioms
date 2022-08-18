module Foldr where
import qualified Data.Set as S
import Data.List

makeSet :: Integer -> S.Set Integer
makeSet x = foldr S.insert S.empty [1 .. x]

reverse :: [Integer] -> [Integer]
reverse = foldl' (\acc x -> x : acc) []

reverse :: [Integer] -> [Integer]
reverse = foldl' flip (:) []
