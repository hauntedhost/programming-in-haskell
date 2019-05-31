import Prelude hiding (filter)

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs
