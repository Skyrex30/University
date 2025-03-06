-- 18, [2,5,10]
-- (2, K2) - K2=[0..9]      18 `div` 2
-- --> 18-2*K2   -> ossz, [5,10]  ......
-- megallas (0, [])

ermek:: Int -> [Int] -> [[(Int, Int)]]
ermek 0 [] = [[]]
ermek _ [] = []
ermek ossz (e:es) = [(e, k2):mes |
    k2 <- [0..ossz `div` e],
    mes <- ermek (ossz - e*k2) es
    ]

-- kiralynok
queen:: Int -> [[Int]]
queen n = queen_ [1..n]
    where
        queen_ [] = [[]]
        queen_ l = [ b:pn | (b,bn) <- select l, pn <- queen_ bn, not (uti b pn)]

uti:: (Enum a, Eq a, Num a) => a -> [a] -> Bool
uti b xs = or [ i == abs(b-e) | (i,e) <- zip [1..] xs]


select:: [a] -> [(a, [a])]
select [a] = [(a, [])]
select (x:xs) = (x,xs):[(b, x:bm) | (b, bm) <- select xs]

-- or
or':: [Bool] -> Bool
or' l = foldr (||) False l









