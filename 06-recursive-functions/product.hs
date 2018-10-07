product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product' ns
