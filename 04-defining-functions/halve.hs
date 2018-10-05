halve :: [a] -> ([a], [a])
halve [] = error "empty list"
halve xs
  | isOddLength = error "odd length list"
  | otherwise = (take halfLength xs, drop halfLength xs)
  where
    isOddLength = odd listLength
    halfLength = listLength `div` 2
    listLength = length xs
