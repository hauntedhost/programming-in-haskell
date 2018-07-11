mysum ns = mysum' ns 0

mysum' [] s = s
mysum' (n:ns) s = mysum' ns (s + n)
