myproduct ns = myproduct' ns 1

myproduct' [] p = p
myproduct' (n:ns) p = myproduct' ns (p * n)
