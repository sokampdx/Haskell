lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number 7!"
lucky x = "Sorry, out of luck."

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x
