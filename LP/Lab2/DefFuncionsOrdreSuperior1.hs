myFoldl::(a->b->a)->a->[b]->a
myFoldl f a [] = a
myFoldl f a (x:xs) = f x (myFoldl f a xs)
