factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]
