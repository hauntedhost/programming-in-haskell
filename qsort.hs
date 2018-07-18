qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

qsortr :: Ord a => [a] -> [a]
qsortr xs = reverse (qsort xs)
-- qsortr [] = []
-- qsortr (x:xs) = qsortr larger ++ [x] ++ qsortr smaller
--   where
--     smaller = [a | a <- xs, a <= x]
--     larger = [b | b <- xs, b > x]
