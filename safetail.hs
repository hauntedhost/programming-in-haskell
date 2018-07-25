safetail1 :: [a] -> [a]
safetail1 xs =
  if null' xs
    then []
    else tail xs

safetail2 :: [a] -> [a]
safetail2 xs
  | null' xs = []
  | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

null' :: [a] -> Bool
null' [] = True
null' _ = False
