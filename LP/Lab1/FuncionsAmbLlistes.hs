myLength::[Int]->Int
myLength [] = 0
myLength (x:xs) = (myLength xs) + 1

myMaximum::[Int]->Int
myMaximum [x] = x
myMaximum (x:xs)
  | x > max = x
  | otherwise = max
  where max = myMaximum xs

addArray::[Int]->Int
addArray [x] = x
addArray (x:xs) = addArray xs + x

average::[Int]->Float
average [x] = fromIntegral x
average (x:xs) = fromIntegral(addArray (x:xs)) / fromIntegral(myLength (x:xs))

reverse'::[Int]->[Int]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

buildPalindrome::[Int]->[Int]
buildPalindrome x = reverse' x ++ x

removeFromList::[Int]->Int->[Int]
removeFromList [] y = []
removeFromList (x:xs) y =
  if x == y then removeFromList xs y
  else [x] ++ removeFromList xs y

remove::[Int]->[Int]->[Int]
remove x [] = []
remove x (y:ys) = remove (removeFromList x y) ys
