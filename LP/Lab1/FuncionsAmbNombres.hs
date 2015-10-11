absValue::Int->Int
absValue a
	| a < 0     = a*(-1)
	| otherwise = a

power::Int->Int->Int
power b 0 = 1
power b e = b * power b (e-1)

isPrimeAux::Int->Int->Bool
isPrimeAux a b
	| b == 1 = True
	| otherwise = if mod a b == 0 then False
			else isPrimeAux a (b-1)

isPrime::Int->Bool
isPrime a
	| a <= 1    = False
	| otherwise = isPrimeAux a (a-1)

slowFib::Int->Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n-1) + slowFib (n-2)

fib2 0 = (0,1)
fib2 n = (y,(x+y))
	where (x,y) = fib2 (n-1)

quickFib::Int->Int
quickFib n = fst(fib2 n)
