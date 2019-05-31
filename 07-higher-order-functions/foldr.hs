import Prelude hiding (and, foldr, length, or, product, reverse, sum)

-- Many functions that take lists as their arguments can be defined using
-- the following simple pattern of recursion:
--
--     f [] = v
--     f (x:xs) = x # f xs
--
-- Where the function maps the empty list to value `v`, and any non-empty
-- list to an operator `#` applied to the head of the list and the result
-- of recursively processing the tail.
--
--    sum [] = 0
--    sum (x:xs) = x + sum xs
--
--    product [] = 1
--    product (x:xs) = x * product xs
--
--    or [] = False
--    or (x:xs) = x || or xs
--
--    and [] = True
--    and (x:xs) = x && and xs
--
-- The higher-order library function `foldr` (fold right) encapsulates this
-- pattern of recursion with the operator `#` and the value `v` as arguments.
--
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v [] = v
foldr f v (x:xs) = f x (foldr f v xs)

sum :: Num a => [a] -> a
-- sum = foldr (\a b -> a + b) 0
sum = foldr (+) 0

product :: Num a => [a] -> a
-- product = foldr (\a b -> a * b) 1
product = foldr (*) 1

or :: [Bool] -> Bool
or = foldr (||) False

and :: [Bool] -> Bool
and = foldr (&&) True

length :: [a] -> Int
length = foldr (\_ n -> 1 + n) 0

-- | Add element to end of list, "reverse cons"
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse :: [a] -> [a]
-- reverse = foldr (\a b -> snoc a b) []
reverse = foldr snoc []
