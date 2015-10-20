eql::[Int]->[Int]->Bool
eql [] [] = True
eql [] (y:ys) = False
eql (x:xs) [] = False
eql (x:xs) (y:ys)
  | length (x:xs) /= length (y:ys) = False
  | otherwise = all (== True) (zipWith (==) (x:xs) (y:ys))

prod::[Int]->Int
prod [] = 0
prod (x:xs) = foldr (*) 1 (x:xs)

prodOfEvens::[Int]->Int
prodOfEvens [] = 0
prodOfEvens (x:xs) =
  if array /= [] then
    foldr (*) 1 array
  else
    0
  where array = (filter (even) (x:xs))

powersOf2::[Int]
powersOf2 = iterate (2*)1

scalarProduct::[Float]->[Float]->Float
scalarProduct [] [] = 0
scalarProduct (x:xs) (y:ys) = foldr (+) 0 (zipWith (*) (x:xs) (y:ys))
