


-- p23
import System.Random
rnd_select :: RandomGen g => [a] -> Int -> g -> ([a], g)
rnd_select _ 0 gen = ([], gen)
rnd_select [] _ gen = ([], gen)
rnd_select x n gen 
	| n == (length x) = (x, gen)
	| otherwise = rnd_select (removeAt x (k+1)) n gen2
	where (k, gen2) = randomR (0, (length 1) - 1) gen

rnd_selectIO :: [a] -> Int -> IO [a]
rnd_selectIO x n = getStdRandom $ rnd_select x n


-- p22
range :: Int -> Int -> [Int]
range start end
	| start > end = []
	| start == end = [end]
	| otherwise = start:(range (start+1) end)

p22i = range 4 9
p22o = [4,5,6,7,8,9]
test22 = p22i == p22o


-- p21
insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt x (y:ys) n
	| n > length(y:ys) = (y:ys) ++ [x]
	| n <= 1 = x:(y:ys)
	| otherwise = y:(insertAt x ys (n-1))

p21i = insertAt 'X' "abcd" 2
p21o = "aXbcd"
test21 = p21i == p21o


-- p20
removeAt :: Int -> [a] -> (a, [a])
removeAt n (x:xs) 
	| n == 1 = (x, xs)
	| otherwise = (ys, x:zs)
		where (ys, zs) = removeAt (n-1) xs

p20i = removeAt 2 "abcd"
p20o = ('b', "acd")
test20 = p20i == p20o

