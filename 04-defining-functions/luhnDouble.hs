luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = sumIsDivisibleBy 10
  where
    luhnSum = sum [luhnDouble a, b, luhnDouble c, d]
    sumIsDivisibleBy n = luhnSum `mod` 10 == 0

luhnDouble :: Int -> Int
luhnDouble n
  | doubled > 9 = doubled - 9
  | otherwise = doubled
  where
    doubled = n * 2
