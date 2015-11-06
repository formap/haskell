myFoldl::(a->b->a)->a->[b]->a
myFoldl f a [] = a
myFoldl f a (x:xs) = myFoldl f (f a x) xs -- el (f a x) se calcula primero

myFoldr::(a->b->b)->b->[a]->b
myFoldr f a [] = a
myFoldr f a (x:xs) = f x (myFoldr f a xs)

myIterate::(a->a)->a->[a]
myIterate f a = a : iterate f (f a)

myUntil::(a->Bool)->(a->a)->a->a
myUntil c f x
  | c x == True = x
  | otherwise = myUntil c f (f x)

myMap::(a->b)->[a]->[b]
myMap f [] = []
myMap f (x:xs) = [f a | a <- (x:xs)]

myFilter::(a->Bool)->[a]->[a]
myFilter f [] = []
myFilter f (x:xs) = [x | x <- (x:xs), f x == True]

myAll::(a->Bool)->[a]->Bool
myAll f x = myFoldr (\x y -> (f x) && y) True x

myAny::(a->Bool)->[a]->Bool
myAny f x = myFoldr (\x y -> (f x) || y) False x

myZip::[a]->[b]->[(a,b)]
myZip [] [] = []
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith::(a->b->c)->[a]->[b]->[c]
myZipWith f x y = [f x y | (x, y) <- zip x y]
