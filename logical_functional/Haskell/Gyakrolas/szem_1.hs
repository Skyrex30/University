append [] l = l
append (x:xs) l = x:append xs l

nth k (x:xs)
    |k = 0 = x
    |k > 0 = nth (k-1) xs
--nth :: Int -> [a] -> a

dd [] = []
dd (x:xs) = x:x:dd xs
--dd :: [t] -> [t]

kszore 0 _ = []
kszore k e = e:kszore (k-1) e
--kszore :: Int -> t -> [t]

kszor _ [] = []
kszor k x:xs 
    |k > 0 = append (kszore k x) (kszor k xs)
--kszor :: int -> [t] -> [t] 

torolk k l 
    |k > 0 = torolk2 n k l
torolk2 _ _ [] = []
torolk2 n k (x:xs)
    |k = 1 = torolk2 n n xs
    |k > 1 = x:torolk2 n (k-1) xs

take::Int->[t]->[t]
take n (x:xs) 
    |n>0 = x:take (n-1) xs
    |otherwise = []
take _ [] = []

--drop::int->[t]->[t]
drop n (x:xs)
    |n>0 = drop (n-1) xs
    |otherwise = x:xs
drop _ [] = []

torolkv2 k l = take (k-1) l ++ torol k drop k l


--jegyzetet elolvasni a files menuben: CsatoEgri_LogikaiFunkcionalis.pdf