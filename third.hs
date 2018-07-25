third1 :: [a] -> a
third1 xs
  | length xs <= 2 = error "list is too small"
  | otherwise = head (tail (tail xs))

third2 :: [a] -> a
third2 xs
  | length xs <= 2 = error "list is too small"
  | otherwise = xs !! 2

third3 :: [a] -> a
third3 (_:_:x:_) = x
third3 _ = error "list is too small"
