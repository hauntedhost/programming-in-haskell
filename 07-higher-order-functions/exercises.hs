import Prelude hiding
  ( all
  , any
  , curry
  , dropWhile
  , filter
  , map
  , takeWhile
  , uncurry
  )

-- define map with foldr
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x : xs) []

-- define filter with foldr
filter :: (a -> Bool) -> [a] -> [a]
filter f =
  foldr
    (\x xs ->
       case f x of
         True -> x : xs
         False -> xs)
    []

-- re-express this:
--     [f x | x <- xs | p x]
-- using map and filter
-- example:
--     mapFilter (+ 3) (> 5) [1, 3, 5, 9, 13]
mapFilter :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapFilter f p = map f . filter p

-- decide if all elements in a list satisfy a predicate
all :: (a -> Bool) -> [a] -> Bool
all p xs = all' p xs True

all' :: (a -> Bool) -> [a] -> Bool -> Bool
all' _ [] b = b
all' _ _ False = False
all' p (x:xs) _ = all' p xs (p x)

-- decide if any element in a list satisfies a predicate
any :: (a -> Bool) -> [a] -> Bool
any p xs = any' p xs False

any' :: (a -> Bool) -> [a] -> Bool -> Bool
any' _ [] b = b
any' _ _ True = True
any' p (x:xs) _ = any' p xs (p x)

---
-- take elements while predicate is satisfied
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p xs = takeWhile' p xs []

takeWhile' :: (a -> Bool) -> [a] -> [a] -> [a]
takeWhile' _ [] ys = reverse ys
takeWhile' p (x:xs) ys =
  case p x of
    True -> takeWhile' p xs (x : ys)
    False -> reverse ys

-- remove elements while predicate is satisfied
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p xs = dropWhile' p xs xs

dropWhile' :: (a -> Bool) -> [a] -> [a] -> [a]
dropWhile' _ [] ys = ys
dropWhile' p (x:xs) ys =
  case p x of
    True -> dropWhile' p xs xs
    False -> ys

-- use foldl
dec2int :: [Int] -> Int
dec2int = foldl (\a b -> a * 10 + b) 0

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f = (\a -> (\b -> f (a, b)))

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f = (\(a, b) -> f a b)

-- TODO:
-- 6. redefine the functions chopd8, map f and iterate f using unfold
-- 7. modify the string transmitter to detect simple transmission errors
-- 8. test new string transmitter
-- 10. define luhn using altMap
--
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = altMap' (f, g) xs []

altMap' :: ((a -> b), (a -> b)) -> [a] -> [b] -> [b]
altMap' _ [] ys = ys
altMap' (f, g) (x:xs) ys = altMap' (g, f) xs zs
  where
    zs = (f x) : ys
--
-- altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
-- altMap f g xs = altMap' (f, g) xs
--
-- altMap' :: ((a -> b), (a -> b)) -> [a] -> [b]
-- altMap' _ [] = []
-- altMap' (f, g) (x:xs) = (f x) : altMap' (g, f) xs
