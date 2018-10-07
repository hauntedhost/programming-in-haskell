import Prelude hiding ((!!))

(!!) :: [a] -> Int -> a
[] !! _ = error "index too large"
(x:_) !! 0 = x
(_:xs) !! n
  | n > 0 = xs !! (n - 1)
  | otherwise = error "negative index"
