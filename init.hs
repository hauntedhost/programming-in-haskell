-- myinit [] = error "empty list"
-- myinit xs = reverse (tail (reverse xs))
--
myinit [] = error "empty list"
myinit xs = take (length xs - 1) xs
