
module Example2 where

scopeTest :: Integer
scopeTest = 
	let x = 3 in
		let f1 = x
		    f2 = 
		    	let x = 4 in
		    		f1			in
		f2
		    
			
f x = "I am not using argument x"

intsFrom :: Integer -> [Integer]
intsFrom n = n : (intsFrom (n+1))

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

sieve (n:ns) = n: sieve (filter (notDiv n) ns) where
	notDiv n x = x `mod` n /= 0
	
primes = sieve [2..]

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show

infPower2 = 1 : 2 : map (* 2) (tail infPower2)

infTree :: a -> Tree a


-- | takeTree :: Integer -> Tree a -> Tree a
