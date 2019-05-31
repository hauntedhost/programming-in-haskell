import Prelude hiding (and, foldl, length, or, product, reverse, sum)

-- Many functions on lists can be defined using the following simple
-- pattern of recursion:
--
--    f v [] = v
--    f v (x:xs) = f (v # x) xs
--
-- The function maps the empty list to the accumulator value `v` and any
-- non-empty list to the result of recursively processing the tail using a
-- new accumulator value obtained by applying operator `#` to the current
-- value; the head of the list
--
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ v [] = v
foldl f v (x:xs) = foldl f (f v x) xs

-- sum :: Num a => [a] -> a
-- sum = sum' 0
--   where
--     sum' v [] = v
--     sum' v (x:xs) = sum' (v + x) xs
sum :: Num a => [a] -> a
sum = foldl (+) 0

product :: Num a => [a] -> a
product = foldl (*) 1

or :: [Bool] -> Bool
or = foldl (||) False

and :: [Bool] -> Bool
and = foldl (&&) True

length :: [a] -> Int
length = foldl (\n _ -> n + 1) 0

reverse :: [a] -> [a]
reverse = foldl (\xs x -> x : xs) []
