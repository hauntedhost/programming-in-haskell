-- | A function that takes a function and a value and returns result of
-- applying the function twice to the value
twice :: (a -> a) -> a -> a
twice f x = f (f x)
