import Data.List

type Vote = String

votes :: [Vote]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

--count :: Eq a => a -> [a] -> Int
count :: Vote -> [Vote] -> Int
count x = length . filter (== x)

--
-- rmdups [1, 5, 5, 3, 2, 1]
-- 1 : filter (/= 1) (rmdups [5, 5, 3, 2, 1])
--   rmdups [5, 5, 3, 2, 1]
--   5 : filter (/= 5) (rmdups [5, 3, 2, 1])
--     rmdups [5, 3, 2, 1]
--     5 : filter (/= 5) (rmdups [3, 2, 1])
--       rmdups [3, 2, 1]
--       3 : filter (/= 3) (rmdups [2, 1])
--         rmdups [2, 1]
--         2 : filter (/= 2) (rmdups [1])
--           rmdups [1]
--           1 : filter (/= 1) (rmdups [])
--             rmdups []
--             []
--
-- rmdups :: Eq a => [a] -> [a]
rmdups :: [Vote] -> [Vote]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

--
-- example:
--   > result votes
--   [(1, "Green"), (2, "Red"), (3, "Blue")]
--
-- result :: Ord a => [a] -> [(Int, a)]
result :: [Vote] -> [(Int, Vote)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: [Vote] -> Vote
winner = snd . last . result

-- runoff voting
ballots :: [[Vote]]
ballots =
  [ ["Red", "Green"]
  , ["Blue"]
  , ["Green", "Red", "Blue"]
  , ["Blue", "Green", "Red"]
  , ["Green"]
  ]

rmempty :: [[Vote]] -> [[Vote]]
rmempty = filter (/= [])

elim :: Vote -> [[Vote]] -> [[Vote]]
elim x = map (filter (/= x))

--
-- rank ballots
-- ["Red", "Blue", "Green"]
--
rank :: [[Vote]] -> [Vote]
rank = map snd . result . map head

winner' :: [[Vote]] -> Vote
winner' bs =
  case rank (rmempty bs) of
    [] -> "unknown rank"
    [c] -> c
    (c:_) -> winner' (elim c bs)
