import Prelude hiding (last)

last :: [a] -> a
last [x] = x
last (_:xs) = last xs
