
multThree :: (Num a) => a -> a -> a -> a
multThree  x y z = x * y * z


largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0


largestDivisible' :: (Integral a) => a -> a -> a
largestDivisible' x y = head (filter p [x, (x-1)..])
    where p z = z `mod` y == 0
