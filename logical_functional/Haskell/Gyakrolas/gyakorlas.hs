import Data.List
--2015. 4
sulyozott_osszeg :: [(Double, Double)] -> Double
sulyozott_osszeg xs = sum (map (\(grade, w) -> grade * w) xs) / sum (map snd xs)

-- 5.a
select :: Eq a => a -> [a] -> [a]
select _ [] = []
select a (x:xs)
    | a == x = xs
    | otherwise = x : select a xs 

sseell2 (l:ls) (r:rs)
    | l==r = sseell2 ls rs
    |  otherwise = ell l ls (r:rs)
    where
        ell _ _ [] = Nothing
        ell a b c
            |b == c = Just a
            |otherwise = Nothing

permutacio :: [a] -> [[a]]
permutacio [] = [[]]  -- Az üres lista permutációja csak az üres lista.
permutacio (x:xs) = [ insertElem x i tp | i <- [0..length xs], tp <- permutacio xs ]
  where 
    -- `insertElem e i xs` beszúrja az `e` elemet az `i`-edik helyre a `xs` listában.
    insertElem :: a -> Int -> [a] -> [a]
    insertElem e 0 xs = e : xs  -- Ha az index 0, akkor az elem a lista elejére kerül.
    insertElem e i (y:ys) = y : insertElem e (i-1) ys  -- Rekurzív módon csökkentjük az indexet, és hozzárakjuk az `e` elemet a megfelelő helyre.


-- Descartes
cartesian :: Int -> [a] -> [[a]]
cartesian 0 _  = [[]]  -- Base case: one empty list for 0 dimensions
cartesian n xs = [x : ys | x <- xs, ys <- cartesian (n - 1) xs]

data Kif = E Double | O Kif Op Kif
  deriving Show

data Op = Plusz | Minusz | Szor | Oszt
  deriving Show

-- Function to evaluate an expression
ertekel :: Kif -> Double
ertekel (E z) = z
ertekel (O bal op jobb) =
  let vb = ertekel bal
      vj = ertekel jobb
  in case op of
       Plusz  -> vb + vj
       Minusz -> vb - vj
       Szor   -> vb * vj
       Oszt   -> if vj == 0 then 1e56 else vb / vj


epit [x] [] = E x
epit (x:xs) (op:ops) =
    O (E x) op (epit xs ops)

uj ert opok = [epit kaga macska | kaga <- permutacio(ert), macska <- cartesian (length opok) (opok)]


zipWith2:: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2 _ [] [] = []
zipWith2 _ _ [] = []
zipWith2 _ [] _ = []
zipWith2 op (x:xs) (y:ys) = op x y : (zipWith2 op xs ys)

fuggS lista = zipWith(\a b -> [a, b]) lista (tail lista)

(~~) :: [a] -> [Int] -> [a]
(~~) _ [] = []
(~~) [] _ = []
xs ~~ indices = [xs !! (i) | i <- indices]

iff:: Bool -> a -> a -> a
iff True x _ = x
iff False _ y = y

iff2:: Bool -> a -> a -> a
iff2 x y z
    | x == True = y
    | x == False = z

--2022
--4

drop2:: Int -> [a] -> [a]
drop2 1 (_:xs) = xs
drop2 n (x:xs)
    | n > 0 = drop2 (n - 1) xs
drop2 _ _ = []

descartes:: Int -> [a] -> [[a]]
descartes 0 _ = [[]]
descartes n xs = [ x : y | x <- xs, y <- descartes (n - 1) xs]

select3:: Eq a => a -> [a] -> [a]
select3 _ [] = []
select3 n (x:xs)
    | n == x = xs
    | otherwise = x : select3 n xs

--2023
dupParos:: Integral a => [a] -> [a]
dupParos [] = []
dupParos (x:xs)
    | x `mod` 2 == 0 = x:x:dupParos xs
    | otherwise = dupParos xs

dupParos2 = (foldr (\x l -> x:x:l) []) . filter(\x -> x `mod` 2 == 0)

max100alatt:: (Num a, Ord a) => [a] -> Maybe a
max100alatt lista 
    | van /= [] = Just (last(sort van))
    | otherwise = Nothing
    where
        van = filter(\x -> x < 100) lista

max100alatt2:: Integral a => [a] -> a 
max100alatt2 = last . sort . filter(\x -> x < 100)

f :: ([a] -> [b] -> c) -> [a] -> [b] -> [c]
f _ [] _ = []
f _ _ [] = []
f a b c = a (init b) (init $ take (length b) c) : f a (tail b) (tail c)

type Coord = (Float, Float)
type Radius = Float
type Gear = (Coord, Radius)
type Gears = [Gear]

tav :: Coord -> Coord -> Float
tav (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

touches:: Gears -> Gear -> Gears
touches [] _ = []
touches (x:xs) gear
    | tav (fst x) (fst gear) > ( snd x + snd gear) = touches xs gear
    | otherwise = x : touches xs gear

rosszul :: Gears -> Bool
rosszul [] = False
rosszul (x:xs)
    | touches xs x == [] = rosszul xs
    | otherwise = True

fg gears = not . null . touches gears

i True x _ = x
i _ _ x = x

j (x:xs) = foldr(\x y -> i (x > y) x y) x xs

megfordit:: [a] -> [a]
megfordit = foldr(\x acc -> acc ++ [x]) []

--2024

a b c = b : a c (b + c)


--4.
init_dict :: (Num b, Enum a, Enum b) => a -> a -> [(a, b)]
init_dict x y = zip [x..y] [0,0..]

fill_dict:: (Eq a) => [(a, Int)] -> [a] -> [(a, Int)]
fill_dict (x:xs) list = (fst x, szamol list (fst x)) : fill_dict xs list

szamol::(Eq a, Num b) => [a]-> a -> b
szamol [] _ = 0
szamol (x:xs) e
    | x == e = 1 + szamol xs e
    | otherwise = szamol xs e

rend_gen:: (Eq a) => [(a, Int)] -> [a]
rend_gen list = concat (map (\(x, y) -> replicate y x) list)

konyvtar_rend input = rend_gen (fill_dict (init_dict (minimum input) (maximum input)) input)

greater True x _ = x
greater _ _ x  = x

maxi (x:xs) = foldr(\x  y-> greater (x > y) x y) x xs

--5.
uj_fuggv:: (Ord a) => [a] -> Int
uj_fuggv = snd . head . reverse . sort . (flip zip [1..])

foldr' :: a -> (a -> a -> a) -> [a] -> a
foldr' a _ [] = a 
foldr' kezd op (x:xs) = op x (foldr' kezd op xs)

toSet :: (Eq a) => [a] -> [a]
toSet [] = []
toSet (x:xs)
    | x `elem` xs = toSet xs
    | otherwise = x : toSet xs

intersect2:: (Eq a) => [a] -> [a] -> [a]
intersect2 x y = inter (toSet x) (toSet y)
    where
        inter [] _ = []
        inter (x:xs) ys
            | x `elem` ys = x : inter xs ys
            | otherwise = inter xs ys
rev::[a] -> [a]
rev = foldl (\ l x -> x:l) []

-- 2019
iperm xs = map snd (sort (zip xs [1..]))

iperm2 :: [Int] -> [Int]
iperm2 = map snd . sort . flip zip [1..]

zip2:: [a] -> [a] -> [(a, a)]
zip2 (x:xs) (y:ys) = (x, y) : zip2 xs ys

data Fa a = 
    Nodus (Fa a) a (Fa a)
    | Level

beszur:: Ord a => Fa a -> a -> Fa a
beszur Level ertek = Nodus (Level) ertek (Level)

beszur (Nodus (balFa) ertek (jobbFa)) ujErtek
    | ujErtek > ertek = Nodus(balFa) ertek (beszur jobbFa ujErtek)
    | ujErtek < ertek = Nodus(beszur balFa ujErtek) ertek (jobbFa)
    | otherwise = Nodus balFa ertek jobbFa

--7.

all2 _ [] = True
all2 cond (l:ls)
    | cond l = all2 cond ls
    | otherwise = False


-- 2015.

sulyozott::[(Double, Double)] -> Double
sulyozott [] = 0
sulyozott xs = sum (map(\(grade, w) -> grade * w) xs) / sum(map snd xs)

select4:: Eq a => a -> [a] -> [a]
select4 x (l:ls) 
    | x == l = ls
    | otherwise = l : select4 x ls

select'' [x] [] = Just x
select'' _ [] = Nothing
select'' [] _ = Nothing
select'' (x:xs) (y:ys)
    | x == y = select'' xs ys
    | otherwise = ellenoriz x xs (y:ys)
    where
        ellenoriz x xs l
            | xs == l = Just a
            | otherwise = Nothing

inverseSelect og result = missing
  where
    missing = head [x | x <- og, x `notElem` result]

inverseSelect2:: Eq a => [a] -> [a] -> Maybe a
inverseSelect2 (x:_) [] = Just x
inverseSelect2 xs ys
    | length xs /= 1 + length ys = Nothing
inverseSelect2 (x:xs) (y:ys)
    | x == y = inverseSelect2 xs ys
    | otherwise = Just x

insertAt::Int -> a -> [a] -> [a]
insertAt _ x [] = [x]
insertAt pos elem (x:xs)
    | pos == 0 = elem : (x:xs) 
    | otherwise = x : insertAt (pos - 1) elem xs

permuta::[a] -> [[a]]
permuta [] = [[]]
permuta (x:xs) = [insertAt pos x tp | pos <- [0..length xs], tp <- permuta xs]

szorzat:: Int -> [a] -> [[a]]
szorzat 0 _ = [[]]
szorzat n xs = [x : ys | x <- xs, ys <- szorzat (n -1) xs]

fact 0 = 1
fact n = n * fact(n - 1)


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
