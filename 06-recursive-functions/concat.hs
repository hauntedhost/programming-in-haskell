import Prelude hiding (concat)

concat :: [[a]] -> [a]
concat [] = []
concat (xs:ys) = xs ++ concat ys
