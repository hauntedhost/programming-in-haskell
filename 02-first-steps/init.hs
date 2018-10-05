-- | Return all elements from given List except the last
myinit :: [a] -> [a]
myinit [] = error "empty list"
myinit xs = take (length xs - 1) xs
--
-- myinit [] = error "empty list"
-- myinit xs = reverse (tail (reverse xs))
