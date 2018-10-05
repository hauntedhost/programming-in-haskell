pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)
