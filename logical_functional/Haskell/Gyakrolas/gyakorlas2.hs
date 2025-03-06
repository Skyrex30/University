import Data.List


--2019 jan 17
zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2 _ [] _ = []
zipWith2 _ _ [] = []
zipWith2 f (x: xs) (y:ys) = f x y : zipWith2 f xs ys 

(~~~) :: [a] -> [Int] -> [a]
_ ~~~ [] = []
xs ~~~ (n:nums) = (xs !! n) : (xs ~~~ nums)

iff :: Bool -> a -> a -> a
iff f a b 
    | f == True = a
    | f == False = b

data Kif = E Double | O Kif Op Kif
  deriving Show

data Op = Plusz | Minusz | Szor | Oszt
  deriving Show

ertekel:: Kif -> Double
ertekel (E z) = z
ertekel (O bal op jobb) =
  let vb = ertekel bal
      vj = ertekel jobb
  in case op of
       Plusz  -> vb + vj
       Minusz -> vb - vj
       Szor   -> vb * vj
       Oszt   -> if vj == 0 then 1e56 else vb / vj

epit:: [Double] -> [Op] -> Kif 
epit [x] [] = E x
epit (x:xs) (op:ops) =
    O (E x) op (epit xs ops)

descartes:: Int -> [b] -> [[b]]
descartes 0 _ = [[]]
descartes n xs = [x : ys | x <- xs, ys <- descartes (n-1) xs]

osszesKifejezes:: Int -> [Double] -> [Op] -> [Kif]
osszesKifejezes n nums ops = [epit numPerm opPerm | numPerm <- permutations nums, opPerm <- descartes (n-1) ops]

--2023
dupParos:: [Int] -> [Int]
dupParos (x:xs)
    | x `mod` 2 == 0 = x:x:dupParos(xs)
    | otherwise = dupParos(xs)

dupParos2 xs = foldr(\x acc -> x:x:acc) [] (filter (\x -> x `mod` 2 == 0))

max100alatt:: (Num a, Ord a) => [a] -> Maybe a
max100alatt xs 
    | van /= [] = Just(last(sort van))
    | otherwise = Nothing
    where 
        van = filter(\x -> x < 100) xs
f:: ([a] -> [b] -> c) -> [a] -> [b] -> [c]
f a b c = a (init b) (init c) : f a (tail b) (tail c)

type Coord = (Float, Float)
type Radius = Float
type Gear = (Coord, Radius)
type Gears = [Gear]

tav :: Coord -> Coord -> Float
tav (x1, y1) (x2, y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)

touches :: Gears -> Gear -> Gears
touches [] _ = []
touches (x:xs) gear
    | tav (fst x) (fst gear) > (snd x + snd gear) = touches xs gear
    | otherwise = x : touches xs gear

rosszul :: Gears -> Bool
rosszul [] = False
rosszul (x:xs)
    | touches xs x == [] = rosszul xs
    | otherwise = True

fg:: Gears -> Gear ->Bool
fg gears = not. null . touches gears
