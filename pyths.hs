-- | Return Pythagorean triples up to given n
pyths :: Int -> [(Int, Int, Int)]
pyths n =
  [ (x, y, z)
  | x <- [1 .. n]
  , y <- [1 .. n]
  , z <- [1 .. n]
  , (x ^ (2 :: Int)) + (y ^ (2 :: Int)) == z ^ (2 :: Int)
  ]
