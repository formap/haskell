eql::[Int]->[Int]->Bool
eql [] [] = True
eql [] (y:ys) = False
eql (x:xs) [] = False
eql (x:xs) (y:ys)
  | length (x:xs) /= length (y:ys) = False
  | otherwise = all (== True) (zipWith (==) (x:xs) (y:ys))

prod::[Int]->Int
prod [] = 1
prod (x:xs) = foldr (*) 1 (x:xs)
-- alternative
-- prod = foldr (*) 1

prodOfEvens::[Int]->Int
prodOfEvens [] = 1
prodOfEvens (x:xs) = foldr (*) 1 (filter (even) (x:xs))

powersOf2::[Int]
powersOf2 = iterate (2*)1
-- alternative
-- powersOf2 = [2^i | i <- [0..]]

scalarProduct::[Float]->[Float]->Float
scalarProduct [] [] = 0
scalarProduct (x:xs) (y:ys) = foldr (+) 0 (zipWith (*) (x:xs) (y:ys))
