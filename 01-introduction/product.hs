myproduct :: Num a => [a] -> a
myproduct ns = myproduct' ns 1

myproduct' :: Num a => [a] -> a -> a
myproduct' [] p = p
myproduct' (n:ns) p = myproduct' ns (p * n)
