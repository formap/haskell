myLength::[Int]->Int
myLength [] = 0
myLength (x:xs) = myLength xs + 1
