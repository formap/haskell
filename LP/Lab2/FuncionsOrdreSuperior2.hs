flatten::[[Int]]->[Int]
flatten [] = []
flatten (x:xs) = foldr (++) [] (x:xs)

myLength::String->Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse::[Int]->[Int]
myReverse [] = []
myReverse (x:xs) = foldl (\y z -> z : y) [] (x:xs)

countIn::[[Int]]->Int->[Int]
countIn [] n = []
countIn (x:xs) n = map (\y -> length (filter (==n) y)) (x:xs)

firstWord::String->String
firstWord [] = []
firstWord (x:xs) = head (words (x:xs))
