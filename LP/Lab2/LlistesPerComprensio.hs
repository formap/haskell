{-
myMap::(a->b)->[a]->[b]
myMap _ [] = []
myMap f (x:xs) = f(x) : myMap f xs
-}
myMap::(a->b)->[a]->[b]
myMap _ [] = []
myMap f (x:xs) = [f x | x <- (x:xs)]
{-
myFilter::(a->Bool)->[a]->[a]
myFilter _ [] = []
myFilter f (x:xs)
  | f(x)==True = x : myFilter f xs
  | otherwise = myFilter f xs
-}
myFilter::(a->Bool)->[a]->[a]
myFilter _ [] = []
myFilter f (x:xs) = [x | x <- (x:xs), f x == True]
{-
myZipWith::(a->b->c)->[a]->[b]->[c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
-}
myZipWith::(a->b->c)->[a]->[b]->[c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = [f x y | (x, y) <- zip (x:xs) (y:ys)]

thingify::[Int]->[Int]->[(Int, Int)]
thingify [] _ = []
thingify _ [] = []
thingify (x:xs) (y:ys) = [(x,y) | x <- (x:xs), y <- (y:ys), mod x y == 0]
{-
factorsAux::Int->Int->[Int]
factorsAux x y
  | x == y = [x]
  | mod x y == 0 = y : factorsAux x (y+1)
  | otherwise = factorsAux x (y+1)

factors::Int->[Int]
factors x = factorsAux x 1
-}
factors::Int->[Int]
factors x = [y | y <- [1..x], mod x y == 0]
