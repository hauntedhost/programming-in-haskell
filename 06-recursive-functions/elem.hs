import Prelude hiding (elem)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys)
  | x == y = True
  | otherwise = elem x ys
