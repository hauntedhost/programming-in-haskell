merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x == y = x : y : merge xs ys
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge ys (x : xs)
