



-- p19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate (x:xs) n 
	| n > 0 = rotate (xs++[x]) (n-1)
	| otherwise = rotate (x:xs) (length (x:xs) + n)

p19i1 = rotate "abcdefgh" 3
p19o1 = "defghabc"
p19i2 = rotate "abcdefgh" (-2)
p19o2 = "ghabcdef"
test19 = (p19i1 == p19o1) && (p19i2 == p19o2)



-- p18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) a b
	| a > 1 && b > 0 = slice xs (a-1) (b-1)
	| b > 0 = x:(slice xs 0 (b-1))
	| otherwise = []	

p18i = slice "abcdefghijk" 3 7
p18o = "cdefg"
test18 = p18i == p18o


-- p17
split :: [a] -> Int -> ([a],[a])
split [] _ = ([], [])
split xs n = (fstPart xs n, sndPart xs n)

fstPart [] _ = []
fstPart (x:xs) n
	| n == 0 = []
	| otherwise = x:(fstPart xs (n-1))

sndPart [] _ = []
sndPart (x:xs) n
	| n == 0 = (x:xs)
	| otherwise = sndPart xs (n-1) 

split2 :: [a] -> Int -> ([a],[a])
split2 [] _ = ([], [])
split2 (x:xs) n
	| n > 0 = (x:ys, zs)
	| otherwise = ([], x:xs)
	where (ys, zs) = split2 xs (n-1)

p17i = split "abcdefghik" 3
p17o = ("abc", "defghik")
test17 = p17i == p17o


-- p16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery x 0 = x
dropEvery x n = dropEveryC x n 1

dropEveryC [] _ _ = []
dropEveryC (x:xs) n c
	| n == c = dropEveryC xs n 1
	| otherwise = x:(dropEveryC xs n (c+1))

p16i = dropEvery "abcdefghik" 3
p16o = "abdeghk"
test16 = p16i==p16o


-- p15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli x 1 = x
repli (x:xs) n = (repli1 x n) ++ (repli xs n)

repli1 :: a -> Int -> [a]
repli1 x 0 = []
repli1 x n = [x] ++ (repli1 x (n-1)) 

p15i = repli "abc" 3
p15o = "aaabbbccc"
test15 = p15i==p15o


-- p14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x]++(dupli xs)

p14i = dupli [1, 2, 3] 
p14o = [1,1,2,2,3,3]
test14 = p14i==p14o


-- p13
encodeDirect :: (Eq a) => [a] -> [CountElem a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect1 1 x xs

encodeDirect1 :: (Eq a) => Int -> a -> [a] -> [CountElem a]
encodeDirect1 n x [] = [encodeElem n x]
encodeDirect1 n x xs
	| x == (head xs) = encodeDirect1 (n+1) x (tail xs)
	| otherwise = [(encodeElem n x)] ++ (encodeDirect1 1 (head xs) (tail xs))

encodeElem :: (Eq a) => Int -> a -> CountElem a
encodeElem n x
	| n == 1 = Single x
	| otherwise = Multiple n x

p13i=encodeDirect "aaaabccaadeeee"
p13o=[Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
test13 = p13i == p13o


-- p12
decodeModified :: (Eq a) => [CountElem a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = (convertStr x)++(decodeModified xs)

convertStr :: (Eq a) => CountElem a -> [a]
convertStr (Single s) = buildstr 1 s
convertStr (Multiple n s) = buildstr n s

buildstr :: (Eq a) => Int -> a -> [a]
buildstr n s 
	| n == 0 = []
	| otherwise = s:(buildstr (n-1) s)

p12i=decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
p12o="aaaabccaadeeee"
test12=p12i==p12o


-- p11
data CountElem a 
	= Single a
	| Multiple Int a
	deriving (Show, Eq)
	
encodeModified :: (Eq a) => [a] -> [CountElem a]
encodeModified [] = []
encodeModified x = convertWord (encode x)

convertWord :: (Eq a) => [(Int, a)] -> [CountElem a]
convertWord [] = []
convertWord ((n, s):xs)
	| n == 1 = (Single s):(convertWord xs)
	| otherwise = (Multiple n s):(convertWord xs)

p11i=encodeModified "aaaabccaadeeee"
p11o=[Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
test11 = p11i == p11o


-- p10
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode x = encodePack (pack x)
encodePack [] = []
encodePack (x:xs) = (length x, head x):encodePack xs
-- Alternative
encode2 xs = [(length x, head x) | x <- pack xs] 
encode3 xs = map (\x -> (length x, head x)) (pack xs)

p10i="aaaabccaadeeee"
p10o=[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
test10 = (encode p10i) == p10o


