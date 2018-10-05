-- | Return scalar product of two lists
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x' * y' | (x', y') <- zip xs ys]
