import Prelude hiding ((*))

(*) :: Int -> Int -> Int
m * 0 = 0
m * n = m + (m * (n - 1))
