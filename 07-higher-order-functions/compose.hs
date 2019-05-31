import Prelude hiding ((.), id, odd)

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

odd :: Int -> Bool
odd = not . even

twice :: (a -> a) -> (a -> a)
twice f = f . f

double :: Int -> Int
double n = n * 2

square :: Int -> Int
square = twice double

sumsqreven :: [Int] -> Int
sumsqreven = sum . map (^ 2) . filter even

id :: a -> a
id = \x -> x

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

doubleSquare :: Int -> Int
doubleSquare = compose [double, square]
