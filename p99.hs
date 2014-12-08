import System.Random
import Data.List



-- p24
diff_select n m = rnd_select3 [1..m] n


-- p23
rnd_select3 :: [a] -> Int -> [a]
rnd_select3 xs n = map (xs !!) is
	where is = take n . nub $ randomRs (0, length xs - 1) (mkStdGen 100)

rnd_select2 xs n = do
	g <- newStdGen
	return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) g]


--rnd_select :: [a] -> Int -> [a]
rnd_select xs n = do
	g <- newStdGen
	print $ rndS xs n g

--rndS :: RandomGen g => [a] -> Int -> g -> [a]
rndS [] _ _ = []
rndS xs n g
	| n == 0 = []
	| otherwise = (xs !! r) : rndS xs (n-1) gen
	where (r, gen) = randomR (0, ((length xs) - 1)) g



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
split' :: [a] -> Int -> ([a],[a])
split' [] _ = ([], [])
split' xs n = (fstPart xs n, sndPart xs n)

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

p17i = split' "abcdefghik" 3
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


-- p09
pack [] = []
pack [x] = [[x]]
pack (x:xs)
	| elem x (head (pack xs)) = (x:head (pack xs)):tail (pack xs)
	| otherwise = [x]:pack xs

p09i=['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
p09o=["aaaa","b","cc","aa","d","eeee"]
test09 = (pack p09i) == p09o


-- p08
compress (x:ys@(y:_))  
    | x == y = compress ys
    | otherwise = x : compress ys
compress ys = ys

p08i="aaaabccaadeeee"
p08o="abcade"
test08=(compress p08i) == p08o

-- p07
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x)      = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []

p07i1=Elem 5
p07i2=List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]
p07i3=List []
p07o1=[5]
p07o2=[1,2,3,4,5]
p07o3=[]
p07input = [p07i1,p07i2,p07i3]
p07output = [p07o1, p07o2, p07o3]
test07= (map (flatten) p07input) == p07output


-- p06
-- isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

p06i1=[1,2,3]
p06i2="madamimadam"
p06i3=[1,2,4,8,16,8,4,2,1]
p06o1=isPalindrome p06i1 == False
p06o2=isPalindrome p06i2 == True
p06o3=isPalindrome p06i3 == True
p06output = [p06o1, p06o2, p06o3]
test06 = foldr (&&) True p06output


-- p05
-- myReverse x = reverse x
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

p05i1="A man, a plan, a canal, panama!"
p05i2=[1,2,3,4]
p05o1="!amanap ,lanac a ,nalp a ,nam A"
p05o2=[4,3,2,1]
p05t1=myReverse(p05i1) == p05o1
p05t2=myReverse(p05i2) == p05o2
p05output = [p05t1, p05t2]
test05 = foldr (&&) True p05output


-- p04
-- myLength x = length x
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

p04i1=[123, 456, 789]
p04i2="Hello, world!"
p04o1=3
p04o2=13
p04t1=myLength(p04i1) == p04o1
p04t2=myLength(p04i2) == p04o2
p04output = [p04t1, p04t2]
test04 = foldr (&&) True p04output


-- p03
--elementAt x y = x !! (y - 1)
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)
elementAt _ _ = error "invalid index"

p03i1=elementAt [1,2,3] 2
p03i2=elementAt "haskell" 5
p03o1=2
p03o2='e'
p03t1=p03i1 == p03o1
p03t2=p03i2 == p03o2
p03output = [p03t1, p03t2]
test03 = foldr (&&) True p03output


-- p02
-- myButLast x = last (init x)
myButLast [] = error "empty list"
myButLast (_:[]) = error "list must have at least 2 elements"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

p02i1=myButLast [1,2,3,4]
p02i2=myButLast ['a'..'z']
p02o1=3
p02o2='y'
p02t1=p02i1 == p02o1
p02t2=p02i2 == p02o2
p02output = [p02t1, p02t2]
test02 = foldr (&&) True p02output


-- p01
-- myLast x = last x
myLast [] = error "empty list"
myLast (x:[]) = x 
myLast (_:xs) = myLast xs

p01i1=myLast [1,2,3,4]
p01i2=myLast ['x','y','z']
p01o1=4
p01o2='z'
p01t1=p01i1 == p01o1
p01t2=p01i2 == p01o2
p01output = [p01t1, p01t2]
test01 = foldr (&&) True p01output






