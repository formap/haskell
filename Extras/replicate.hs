replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' x y 
  | x <= 0 = []
  | otherwise = y:replicate' (x-1) y

main = print( replicate' 3 5)
