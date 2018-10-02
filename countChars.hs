count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x' == x]
