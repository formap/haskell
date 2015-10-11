fact:: Integer -> Integer
prod :: Int -> Int -> Int
-- exemple senzill

fact 0 = 1
fact n = n * fact (n-1)

-- exemple ús if i let (amb format al let)

prod n m = 
     if n == 0 then 0
     else let x = div n 2
              y = mod n 2 
    	  in if y == 0 then 2* (prod x m)
       	     else (prod (n-1) m) + m


-- exemple ús de guardes

prod1 n m  
  | n == 0    = 0
  | otherwise = let x = div n 2
                    y = mod n 2 
                in if y == 0 then 2* (prod x m)
                   else (prod (n-1) m) + m

-- exemple ús where (amb format)

prod2 n m = 
     if n == 0 then 0
     else if y == 0 then 2* (prod2 x m)
       	     else (prod2 (n-1) m) + m
     where x = div n 2
    	   y = mod n 2  


-- exemple sense format al where

prod3 n m = 
     if n == 0 then 0
     else if y == 0 then 2* (prod3 x m)
       	  else (prod3 (n-1) m) + m
     where {x = div n 2; 
      y = mod n 2}


-- exemple ús llistes amb pattern matching

sumar [] = 0
sumar ([x]) = x     -- aquesta sobra però és per veure més patrons
sumar ([x,y]) = x+y -- aquesta també sobra 
sumar (x:xs) = x+(sumar xs)

-- exemple ús pattern matching més complicat (llistes i tuples) i ús de guardes

select [] = []
select ((x,y):xs) 
       | x>=y = x:(select xs) 
       | otherwise = y:(select xs) 


-- exemple ús pattern matching al case

sumar2 l = case l of
       	   [] -> 0
           (x:xs) -> x+(sumar2 xs)

-- exemple ús pattern matching al where amb resultat tupla

division :: Int -> Int -> (Int,Int)
division n m 
    | n < m = (0,n)
    | otherwise = (q+1,r)
    where (q,r) = division (n-m) m
