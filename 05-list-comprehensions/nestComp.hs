-- Show how the following list comprehension with two generators:
flat :: [(Int, Int)]
flat = [(x, y) | x <- [1, 2], y <- [3, 4]]

-- can be re-expressed using two comprehensions with single generators:
nest :: [(Int, Int)]
nest = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]
