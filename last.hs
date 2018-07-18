-- mylast [] = error "empty list"
-- mylast xs = head (reverse xs)
--
mylast [] = error "empty list"
mylast [x] = x
mylast (x:xs) = mylast xs
