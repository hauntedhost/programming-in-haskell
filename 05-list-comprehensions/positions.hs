-- | Return list of positions at which a value occurs in a list
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

-- | Redefine positions using find
positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (zip xs [0 ..])

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']
