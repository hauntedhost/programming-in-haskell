fac :: Int -> Int
fac 0 = 1
fac n
  | n >= 1 = n * fac (n - 1)
  | otherwise = error "negative int"
