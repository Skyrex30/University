--Nagy Csongor, 523
fibo :: Integer -> Integer -> Integer -> [Integer]
fibo 0 a b = [a]
fibo n a b = a:fibo (n-1) b (a+b)

--1. feladat
harminc :: IO ()
harminc = do
    print (fibo 30 0 1 !! 29)
--2. feladat
elsoharminc :: IO ()
elsoharminc = do
    print (take 30 (fibo 35 0 1))
    
elso150 :: IO ()
elso150 = do
    print (take 150 (fibo 160 0 1))


--3. feladat

maxKeres :: [Int] -> (Int, Int) -> (Int, Int)
maxKeres [] (legnagyobb, masodikLegnagyobb) = (legnagyobb, masodikLegnagyobb)
maxKeres(x: xs) (legnagyobb, masodikLegnagyobb)
    | x > legnagyobb = maxKeres xs (x, legnagyobb)
    | x > masodikLegnagyobb = maxKeres xs (legnagyobb, x)
    | otherwise = maxKeres xs (legnagyobb, masodikLegnagyobb)

--4. feladat

osszefesul :: [Int] -> [Int] -> [Int]
osszefesul [] ys = ys
osszefesul xs [] = xs
osszefesul(x: xs) (y: ys)
    | x <= y = x:osszefesul xs (y:ys)
    | otherwise = y:osszefesul (x:xs) ys

--5. feladat

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = (head xs == last xs) && isPalindrome (tail (init xs))

--6. feladat
torolK :: Int -> [Int] -> [Int]
torolK k xs
    | k <= 0 = xs
    | otherwise = helper k xs 1
    where
        helper _ [] _ = []
        helper k (y:ys) n
            | n `mod` k == 0 = helper k ys (n+1)
            | otherwise = y:helper k ys (n+1)

--7. feladat
kompakt :: (Eq a) => [a] -> [(a, Int)]
kompakt [] = []
kompakt (x:xs) = count x 1 xs
    where
        count y n [] = [(y, n)]
        count y n (z:zs)
            | y == z = count y (n + 1) zs
            | otherwise = (y, n) : kompakt (z:zs)
        
--8. feladat
kiszur :: Int -> [Int] -> [Int]
kiszur _ [] = []
kiszur n (x:xs)
    | x `mod` n == 0 = kiszur n xs
    | otherwise      = x : kiszur n xs

szita :: [Int] -> [Int]
szita [] = []
szita (x:xs) = x : szita (kiszur x xs)

valaszt :: Int -> Int
valaszt n = primek !! (n - 1)
  where primek = szita [2..]

--9. feladat
egyediTobbszorosok limit osztok = egyediTobbszorosokseged 1 [] limit osztok 
    where
        egyediTobbszorosokseged :: Int -> [Int] -> Int -> [Int] -> [Int]
        egyediTobbszorosokseged n eredmenyek limit osztok
            | n >= limit = eredmenyek
            | any (\x -> n `mod` x == 0) osztok = egyediTobbszorosokseged (n + 1) (n : eredmenyek) limit osztok
            | otherwise = egyediTobbszorosokseged (n + 1) eredmenyek limit osztok

megold :: Int -> [Int] -> Int
megold limit divisors = sum (egyediTobbszorosok limit divisors)
