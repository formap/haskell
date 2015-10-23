flatten::[[Int]]->[Int]
flatten [] = []
flatten (x:xs) = foldr (++) [] (x:xs)

myLength::String->Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs
--myLength (x:xs) = map (\x -> if x == [] then 0 else 1 + myLength xs)

myReverse::[Int]->[Int]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
