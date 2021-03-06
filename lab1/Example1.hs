
module Example1 where
import Prelude hiding (zipWith)
import Data.Char


plus :: Int -> Int -> Int
plus x y = x + y

plusThree :: Int -> Int
plusThree = plus 3

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

square x = x * x

add2 = twice (+1)
quad = twice square

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = (quickSort less) ++ (x : equal) ++ (quickSort more) where
	less = filter (< x) xs
	equal = filter (== x) xs
	more = filter (> x) xs
	
dictionary = ["I", "have", "a", "thing", "for", "Haskell"]

quickSort' :: (a -> a -> Ordering) -> [a] -> [a]
quickSort' _ [] = []
quickSort' c (x : xs) = (quickSort' c less) ++ (x : equal) ++ (quickSort' c more)
	where
		less = filter (\y-> y `c` x == LT) xs
		equal = filter (\y -> y `c` x == EQ) xs
		more = filter (\y -> y `c` x == GT) xs
		
descending x y = compare y x

sortDescending :: Ord a => [a] -> [a]
sortDescending = quickSort' descending

s1 = filter (\x-> x `mod` 2 == 0) [0..100]

s2 = [x | x<-[0..100], x `mod` 2 == 0]

insensitive x y = compare (map toLower x) (map toLower y)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] [] = []
zipWith (f) (x:xs) (y:ys) = (f x y) : zipWith (f) xs ys

factors :: Integer -> [Integer]
factors n = filter (\y-> n `mod` y == 0) [1..n]
