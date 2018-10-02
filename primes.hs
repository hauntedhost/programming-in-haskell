primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], isPrime x]

isPrime :: Int -> Bool
isPrime n = factors n == [1, n]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]
