data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

--breadthFirst::Tree a->[a]
--breadthFirst (Node x Empty Empty) = [x]
--breadthFirst (Node x y z) = x : breadthFirst y ++ breadthFirst z

size::Tree a->Int
size Empty = 0
size (Node x Empty Empty) = 1
size (Node x y z) = 1 + size y + size z

height::Tree a->Int
height Empty = 0
height (Node x Empty Empty) = 1
height (Node x y z) = 1 + height y + 0

equal::Eq a=>Tree a->Tree a->Bool
equal Empty Empty = True
equal _ Empty = False
equal Empty _ = False
equal (Node a b c) (Node x y z)
  | a == x = equal b y == equal c z
  | otherwise = False

isomorphic::Eq a=>Tree a->Tree a->Bool
isomorphic Empty Empty = True
isomorphic _ Empty = False
isomorphic Empty _ = False
isomorphic (Node a b c) (Node x y z) = isomorphic b y && isomorphic c y

preOrder::Tree a->[a]
preOrder (Node x Empty Empty) = [x]
preOrder (Node x y z) = x : preOrder y ++ preOrder z

postOrder::Tree a->[a]
postOrder (Node x Empty Empty) = [x]
postOrder (Node x y z) = postOrder y ++ postOrder z ++ [x]

inOrder::Tree a->[a]
inOrder (Node x Empty Empty) = [x]
inOrder (Node x y z) = inOrder y ++ [x] ++ inOrder z

breadthFirst::Tree a->[a]
--breadthFirst (Node x Empty Empty) = [x]
--breadthFirst (Node x y z) =

build::Eq a=>[a]->[a]->Tree a
build [] [] = Empty
build (x:xs) (y:ys)
  | x == y = let (a, b) = splitAt(y, (y:ys)) in build a b
  |
