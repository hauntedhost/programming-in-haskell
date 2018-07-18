seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act:acts) = do
  x <- act
  xs <- seqn acts
  return (x : xs)
