sum1 :: (Num a) => [a] -> a
sum1 xs = foldl (\acc x -> acc + x) 0 xs

sum2 :: (Num a) => [a] -> a
sum2 = foldr (+) 0
