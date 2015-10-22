insert::[Int]->Int->[Int]
insert [] y = [y]
insert (x:xs) y
  | x > y = [y] ++ (x:xs)
  | otherwise = [x] ++ insert xs y

isort::[Int]->[Int]
isort [] = []
isort (x:xs) = insert (isort xs) x

remove::[Int]->Int->[Int]
remove [] y = []
remove (x:xs) y
  | x == y = xs
  | otherwise = x : (remove xs y)

ssort::[Int]->[Int]
ssort [] = []
ssort (x:xs) = [min] ++ ssort (remove (x:xs) min)
  where min = minimum (x:xs)

merge::[Int]->[Int]->[Int]
merge [] [] = []
merge (x:xs) [] = (x:xs)
merge [] (y:ys) = (y:ys)
merge (x:xs) (y:ys)
  | x <= y = [x] ++ merge xs (y:ys)
  | otherwise = [y] ++ merge (x:xs) ys

msort::[Int]->[Int]
msort [] = []
msort [x] = [x]
msort (x:xs) =
  let (y,z) = splitAt (div (length (x:xs) + 1) 2) (x:xs) in merge (msort y) (msort z)

qsort::[Int]->[Int]
qsort [] = []
qsort (x:xs) =
  let small = qsort [a | a <- xs, a <= x]
      big = qsort [a | a <- xs, a > x]
  in small ++ [x] ++ big

genQsort::Ord a=>[a]->[a]
genQsort [] = []
genQsort (x:xs) =
  let small = genQsort [a | a <- xs, a <= x]
      big = genQsort [a | a <- xs, a > x]
  in small ++ [x] ++ big
