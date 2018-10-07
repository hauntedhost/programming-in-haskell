length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs
