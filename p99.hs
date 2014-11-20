



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






