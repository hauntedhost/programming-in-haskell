-- | Return list of perfect numbers from 1 up to given integer
perfect :: Int -> [Int]
perfect n = [x | x <- [1 .. n], sum (factors x) == x]

-- | Return list of factors for given integer
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n - 1], n `mod` x == 0]
