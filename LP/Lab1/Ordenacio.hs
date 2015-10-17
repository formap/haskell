insert::[Int]->Int->[Int]
insert [] y = [y]
insert (x:xs) y
  | x > y = [y] ++ (x:xs)
  | otherwise = [x] ++ insert xs y

isort::[Int]->[Int]
isort [] = []
isort (x:xs) = insert (isort xs) x

removeFirstOccurrence::[Int]->Int->Bool->[Int]
removeFirstOccurrence [] y z = []
removeFirstOccurrence (x:xs) y z =
  if x == y && z == False then
    removeFirstOccurrence xs y True
  else
    [x] ++ removeFirstOccurrence xs y z

remove::[Int]->Int->[Int]
remove [] y = []
remove (x:xs) y = removeFirstOccurrence (x:xs) y False

ssort::[Int]->[Int]
ssort [] = []
ssort (x:xs) = [minimum (x:xs)] ++ ssort (remove (x:xs) (minimum (x:xs)))

merge::[Int]->[Int]->[Int]
merge [] [] = []
merge (x:xs) [] = (x:xs)
merge [] (y:ys) = (y:ys)
merge (x:xs) (y:ys) =
  if x <= y then
    [x] ++ [y] ++ merge xs ys
  else
    [y] ++ [x] ++ merge xs ys

msort::[Int]->[Int]
msort [] = []
msort (x:xs) =
  let (y,z) = splitAt (div (length (x:xs) + 1) 2) (x:xs) in merge (ssort y) (ssort z)

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
