import Prelude hiding (and)

and :: [Bool] -> Bool
and [] = True
and (False:_) = False
and (_:xs) = and xs
