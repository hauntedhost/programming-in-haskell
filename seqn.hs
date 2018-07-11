seqn [] = return []
seqn (act:acts) = do
  x <- act
  xs <- seqn acts
  return (x : xs)
