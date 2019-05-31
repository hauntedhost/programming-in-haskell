import Data.Char

type Bit = Int

-- in these example binary numbers are writting in reverse order
-- for example:
--
--     1011 = (1 * 1) + (2 * 0) + (4 * 1) + (8 * 1)
--
-- first, a straightforward implementation:
--
-- note that weights is an infinite list of [1, 2, 4, 8, 16 ...]
--
bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
  where
    weights = iterate (* 2) 1

-- next, an implementation that that takes advantage of fact that:
--
--     "converting a list of bits into an integer amounts to replacing each cons
--     by the function that adds its first argument to twice its second argument"
--
-- foldr is needed because our list of bits is in reverse order
--
-- example:
-- bin2int [1,0,1,1]
-- 1 + 2 * 0 = 1
-- 1 + 2 * 1 = 3
-- 0 + 2 * 3 = 6
-- 1 + 2 * 6 = 13
--
bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2 * y) 0

--
-- repeatedly divide the integer by 2 taking the remainder until we hit 0
--
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

--
-- right pad a list of bits with zeroes and take 8
--
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

--
-- encode a string into a list of 8-bit binaries
-- composes map and concat (to flatten) composition of ord, int2bin and make8
--
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

--
-- take first 8 and append on recurse of list without first 8
--
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

--
-- elixir version:
--
--   bits
--   |> chop8()
--   |> Enum.map(fn bs ->
--     bs
--     |> bin2int()
--     |> chr()
--   end)
--
decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

-- encode |> channel |> decode
transmit :: String -> String
transmit = decode . channel . encode

-- identity
channel :: [Bit] -> [Bit]
channel = id
