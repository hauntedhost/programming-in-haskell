-- | Mutual recursion
even :: Int -> Bool
even 0 = True
even n = odd (n - 1)

even :: Int -> Bool
even 0 = False
even n = even (n - 1)
