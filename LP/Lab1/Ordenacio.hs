insert::[Int]->Int->[Int]
insert [] y = [y]
insert (x:xs) y
  | x > y = [y] ++ (x:xs)
  | otherwise = [x] ++ insert xs y

isort::[Int]->[Int]
isort [] = []
isort (x:xs) = insert (isort xs) x
