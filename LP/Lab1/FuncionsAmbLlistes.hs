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
remove [] [] = []
remove [] (y:ys) = []
remove (x:xs) [] = (x:xs)
remove (x:xs) (y:ys) = remove (removeFromList (x:xs) y) ys

flatten::[[Int]]->[Int]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

oddsNevens::[Int]->([Int],[Int])
oddsNevens [] = ([],[])
oddsNevens (x:xs) = (filter odd (x:xs), filter even (x:xs))

isPrimeAux::Int->Int->Bool
isPrimeAux a b
	| b == 1 = True
	| otherwise = if mod a b == 0 then False
			else isPrimeAux a (b-1)

isPrime::Int->Bool
isPrime a
	| a <= 1    = False
	| otherwise = isPrimeAux a (a-1)

isDivisor::Int->Int->Bool
isDivisor x y
  | (mod x y) == 0 = True
  | otherwise = False

primeAndDivisor::Int->Int->[Int]
primeAndDivisor x y
  | x == 1 = []
  | y == 1 = []
  | otherwise =
      if isDivisor x y && isPrime y then
        primeAndDivisor x (y-1) ++ [y]
      else
        primeAndDivisor x (y-1)


primeDivisors::Int->[Int]
primeDivisors 1 = []
primeDivisors n = primeAndDivisor n n
