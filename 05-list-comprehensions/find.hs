-- | Given key and list of (key, value) pairs, return value of all matching keys
--
-- Example:
-- >>> find 'b' [('b', 7), ('a', 9), ('b', 10), ('a', 12)]
-- [7,10]
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']
