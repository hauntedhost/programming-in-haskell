lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']
