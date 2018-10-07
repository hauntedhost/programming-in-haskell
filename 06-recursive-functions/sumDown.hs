sumDown :: Int -> Int
sumDown n = sumDown' n 0

sumDown' :: Int -> Int -> Int
sumDown' 0 s = s
sumDown' n s = sumDown' (n - 1) (n + s)
