import Data.List (permutations)
--2.
cauchy :: (Double -> Double) -> Double -> Double
cauchy f x0 = fst $ until (\(x, y) -> abs (x - y) < 1e-12) (\(x, y) -> (f x, x)) (f x0, x0)

--3.
(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n - 1)

--4.
all2 :: (a -> Bool) -> [a] -> Bool

all2 p [] = True
all2 p (x:xs)
    | p x = all p xs
    | otherwise = False

all3 p xs = foldr(\x acc -> p x && acc) True xs

all4 p xs = and  (map (\x -> p x) xs)

--5.
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

osszesKifejezes nums ops = [epit numPerm opPerm | numPerm <- permutations nums, opPerm <- permutations ops]

--6. 
data One = One

data ExtInt = N Integer | Infinity

data Mood = Woot | Blah

 --2.1

boolToEither :: Bool -> Either One One
boolToEither False = Left One
boolToEither True = Right One

eitherToBool :: Either One One -> Bool
eitherToBool (Left One) = False
eitherToBool (Right One) = True

--2.2
maybeToEither :: Maybe a -> Either a a
maybeToEither Nothing = Left undefined
maybeToEither (Just x) = Right x

eitherToMaybe :: Either a a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x


--2.4

boolToMood :: Bool -> Mood
boolToMood False = Woot
boolToMood True = Blah

