test [] = []
test (x:y:z) = z ++ [x] ++ [y]


-- p11
data CountElem = Single | Multiple
encodeMod :: Eq a => [a] -> [String]
encodeMod [] = []
encodeMod x = encodeModPack (pack x)
encodeModPack (x:xs) 
	| length x == 1 =  encodeModPack xs
	| otherwise = (length x, head x):encodeModPack xs



-- p10
encode2 xs = [(length x, head x) | x <- pack xs] 

encode3 xs = map (\x -> (length x, head x)) (pack xs)

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode x = encodePack (pack x)
encodePack [] = []
encodePack (x:xs) = (length x, head x):encodePack xs



-- p09
pack [] = []
pack [x] = [[x]]
pack (x:xs)
	| elem x (head (pack xs)) = (x:head (pack xs)):tail (pack xs)
	| otherwise = [x]:pack xs

-- p08
compress (x:ys@(y:_))  
    | x == y = compress ys
    | otherwise = x : compress ys
compress ys = ys


-- p07
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x)      = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []

-- p06
-- isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- p05
-- myReverse x = reverse x
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]


-- p04
-- myLength x = length x
myLength [] = 0
myLength (_:xs) = 1 + myLength xs


-- p03
--elementAt x y = x !! (y - 1)
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)
elementAt _ _ = error "invalid index"


-- p02
-- myButLast x = last (init x)
myButLast [] = error "empty list"
myButLast (_:[]) = error "list must have at least 2 elements"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs


-- p01
-- myLast x = last x
myLast [] = error "empty list"
myLast (x:[]) = x 
myLast (_:xs) = myLast xs
