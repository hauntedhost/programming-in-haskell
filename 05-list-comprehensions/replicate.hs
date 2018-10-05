replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [0 .. n]]
