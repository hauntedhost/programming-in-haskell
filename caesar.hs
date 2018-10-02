import Data.Char (chr, isLower, ord)

-- | Crack and decode given caesar-encoded string using frequency analysis
crack :: String -> String
crack xs = decode key xs
  where
    (key:_) = indexesFor (minimum chitab) chitab
    chitab = [chisqr (rotate n freqs) | n <- [0 .. 25]]
    freqs = freqsFor xs

-- | Return indexes in list where element was found
indexesFor :: Eq a => a -> [a] -> [Int]
indexesFor x xs = [i | (x', i) <- zip xs [0 ..], x == x']

-- | Rotate list by n, assuming n is between 0 and list length
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- | Calculate chi-square statistic
chisqr :: [Float] -> Float
chisqr os = sum [((o - e) ^ (2 :: Int)) / e | (o, e) <- zip os es]
  where
    es = expectedFreqs

-- | Zip lowercase letters with freqsFor given string
-- Useful for debugging
freqTableFor :: String -> [(Char, Float)]
freqTableFor xs = zip ['a' .. 'z'] (freqsFor xs)

-- | Return frequencies of each lowercase letter for given string
freqsFor :: String -> [Float]
freqsFor xs = [percentOf (charCount x xs) n | x <- ['a' .. 'z']]
  where
    n = lowersCount xs

-- | Calculate the percentage that n is of m
percentOf :: Int -> Int -> Float
percentOf n m = (fromIntegral n / fromIntegral m) * 100

-- | Return count of lowercase letters
lowersCount :: String -> Int
lowersCount xs = length [x | x <- xs, isLower x]

-- | Return count of how many times x occurs in xs
charCount :: Char -> String -> Int
charCount x xs = length [x' | x' <- xs, x' == x]

-- | Zip lowercase letters with expectedFreqs
-- Useful for debugging
expectedFreqTable :: [(Char, Float)]
expectedFreqTable = zip ['a' .. 'z'] expectedFreqs

-- | List of expected frequencies in order from a to z
expectedFreqs :: [Float]
expectedFreqs =
  [ 8.1
  , 1.5
  , 2.8
  , 4.2
  , 12.7
  , 2.2
  , 2.0
  , 6.1
  , 7.0
  , 0.2
  , 0.8
  , 4.0
  , 2.4
  , 6.7
  , 7.5
  , 1.9
  , 0.1
  , 6.0
  , 6.3
  , 9.0
  , 2.8
  , 1.0
  , 2.4
  , 0.2
  , 2.0
  , 0.1
  ]

-- | Encode given string with integer as secret key using caesar cipher
encode :: Int -> String -> String
encode n xs = [shiftLower n x | x <- xs]

-- | Decode given string with integer secret key
decode :: Int -> String -> String
decode n xs = encode (-n) xs

-- | Shift lowercase characters by n, otherwise just return character
shiftLower :: Int -> Char -> Char
shiftLower n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

-- | Return integer from 0..25 for given lowercase character
let2int :: Char -> Int
let2int c
  | isLower c = ord c - ord 'a'
  | otherwise = error "invalid character"

-- | Return corresponding lowercase character for given integer from 0..25
int2let :: Int -> Char
int2let n
  | elem n [0 .. 25] = chr (ord 'a' + n)
  | otherwise = error "out of range"
