import Prelude hiding (map)

-- | List comprehension map definition
map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

-- | Recursive map definition
map' :: (a -> b) -> [a] -> [b]
map' _f [] = []
map' f (x:xs) = f x : map' f xs
