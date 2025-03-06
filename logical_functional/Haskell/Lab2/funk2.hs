--Nagy Csongor, 523
import Data.Char
import Data.List
import Data.Maybe

--1. feladat

-- eltavolitja a szokozoket es kisbetusse alakitja a szoveget
preprocess :: String -> String
preprocess [] = []
preprocess (x:xs)
    | x == ' ' = preprocess xs
    | otherwise = toLower x : preprocess xs

-- A szöveget m szélességű négyszögbe rendezi
createRectangle [] acc m db
  | db == m = [acc]
  | otherwise = createRectangle [] (acc ++ " ") m (db+1)

createRectangle (x:xs) acc m db
  | db == m = acc : createRectangle (x:xs) [] m 0
  | otherwise = createRectangle xs (acc ++ [x]) m (db+1)

column :: [[Char]] -> [Char]
column [] = []
column rows = concatMap (filter (/= ' ')) (transpose rows)


    --szoveg felosztasa c meretu darabokra
szokozok :: String -> Int -> String
szokozok [] _c = [] 
szokozok lista c = szokozok2 (column [lista] ) c 1

szokozok2 :: String -> Int -> Int -> String
szokozok2 [] _c _db = []
szokozok2 (l:ls) c db
    | db == c   = l : ' '  : szokozok2 ls c 1 
    | otherwise =  l : szokozok2 ls c (db + 1) 


--2. feladat

gombok :: [String] 
gombok = ["1", "aábc2", "deéf3", "ghií4", "jkl5", "mnoóöő6", "pqrs7", "tuúüűv8", "wxyz9", "+ 0", ".,#"]

nagybetus :: [String]
nagybetus = [ map toUpper string | string <- gombok ]

abrazolhato :: String -> Bool
abrazolhato = all eleme
    where
        eleme char = any (elem char) $ osszefuz gombok nagybetus  

osszefuz :: [a] -> [a] -> [a] 
osszefuz lista1 lista2 = lista1 ++ lista2 

oldPhone :: String -> Maybe [(Char, Int)]
oldPhone lista      
    | abrazolhato lista = Just $ concatMap getKarakter lista 
    | otherwise         = Nothing
    where
        getKarakter karakter  
            | isUpper karakter = ('*', 1) : getIndex (toLower karakter)
            | otherwise = getIndex karakter                                             
            where
                getIndex karakter = [ (toChar (1 + getIndexLista karakter), 1 + getIndexKarakter karakter) ]
                    where
                        getIndexLista karakter  = fromJust $ findIndex (elem karakter) gombok 
                        getIndexKarakter karakter = fromJust $ elemIndex karakter $ gombok !! getIndexLista karakter
                        toChar num
                            | num >= 1 && num <= 10  = intToDigit (num `mod` 10) 
                            | otherwise              = '#' -- ha nem szam akkor # lesz

--3. feladat
precizitas :: Integer
precizitas = fst ( until stop iter (0, 1))
    where
        stop (k, szam) = szam / 2 == 0 
        iter (k, szam) = (k+1, szam / 2)

--4. feladat

logaritmus :: (Ord c, Floating c) => c -> c

logaritmus x 
    | abs(x) >= 1  = negate $ logaritmus (1/x) 
    | otherwise    = negate $ third $ until stop iter (1, x - 1, 0)
    where
        third (_x, _y, z) = z
        stop (k, x, ln) = abs (ln - (ln + ((-x) ** k)/k) ) < 1e-10 --adott pontossagnal megallunk
        iter (k, x, ln) = (k+1, x, ln + ((-x) ** k)/k )
     

--5. feladat
cumul_op _op [] = [] 
cumul_op op (x:xs) =  x : cumul_op2 op xs x 
    where 
        cumul_op2 _op [] _elozo = []
        cumul_op2 op (x:xs) elozo = op elozo x : cumul_op2 op xs (op elozo x )

--6. feladat
data BinFa a =
   Nodus (BinFa a) a (BinFa a)
   | Level

-- ures fa
beszur :: Ord a => BinFa a -> a -> BinFa a
beszur Level ertek = Nodus (Level) ertek (Level)

-- nem ures

beszur (Nodus (balFa) ertek (jobbFa)) ujErtek
    | ujErtek > ertek = Nodus(balFa) ertek (beszur jobbFa ujErtek)
    | ujErtek < ertek = Nodus(beszur balFa ujErtek) ertek (jobbFa)
    | otherwise = Nodus balFa ertek jobbFa

-- let tree = Level
--let tree1 = beszur tree 5
--let tree2 = beszur tree1 3
--let tree3 = beszur tree2 7
--let tree4 = beszur tree3 4
--let tree5 = beszur tree4 6


felepitFa :: (Foldable t, Ord a) => t a -> BinFa a

felepitFa xs = foldl (beszur Level) xs

-- felepitFa [15,2,11]

torol :: Ord a => BinFa a -> a -> Maybe (BinFa a)

torol Level torolni = Nothing

torol (Nodus (bal) ertek (jobb)) torolni
    | torolni < ertek = case (torol bal torolni) of
        Just ujFa -> Just (Nodus (ujFa) ertek (jobb))
        Nothing -> Nothing
    | torolni > ertek = case (torol jobb torolni) of
        Just ujFa -> Just (Nodus (bal) ertek (ujFa))
        Nothing -> Nothing
    | otherwise = torol' (Nodus (bal) ertek (jobb) ) torolni

    where

        torol' ( Nodus (Level) ertek (Level) ) _ = Just Level

        torol' ( Nodus (Level) ertek (jobb) ) _ = Just jobb
        torol' ( Nodus (bal) ertek (Level) ) _ = Just bal
        
        --kitoroljuk a bal reszfabol a legnagyobb elemet
        torol' ( Nodus (bal) ertek (jobb) ) _ = case (torol bal (legnagyobbElem bal)) of

                Just ujBalFa -> Just (Nodus (ujBalFa) (legnagyobbElem bal) jobb )
                Nothing -> Nothing

                where
                    legnagyobbElem ( Nodus (_) ertek (Level) ) = ertek
                    legnagyobbElem ( Nodus (_) _ (jobbFa) ) = legnagyobbElem jobbFa


-- 7. feladat


data Komplex = Komplex Double Double

instance Show Komplex 
    where 
        show (Komplex a b) =  show a ++ " + " ++ show b ++ "i" 

instance Num Komplex
    where
        (+) (Komplex a1 b1) (Komplex a2 b2) = Komplex (a1 + a2) (b1 + b2)
        (-) (Komplex a1 b1) (Komplex a2 b2) = Komplex (a1 - a2) (b1 - b2)
        (*) (Komplex a1 b1) (Komplex a2 b2) = Komplex (a1 * a2 - b1 * b2) (a1 * b2 + b1 * a2)

        fromInteger num = Komplex (fromIntegral num) (0)

        negate (Komplex a b) = Komplex (-a) (-b)
        abs (Komplex a b) = Komplex (sqrt $ a^2 + b^2) 0
        signum (Komplex a b) = Komplex (a / sqrt (a^2 + b^2)) (b / sqrt (a^2 + b^2))

instance Fractional Komplex
    where
        (/) (Komplex a1 b1) (Komplex a2 b2) = Komplex (egeszResz k1 k2) (imaginariusResz k1 k2)
            where
                k1 = (Komplex a1 b1)
                k2 = (Komplex a2 b2)
                egeszResz (Komplex a1 b1) (Komplex a2 b2)       = (a1 * a2 + b1 * b2) / (a2^2 + b2^2)
                imaginariusResz (Komplex a1 b1) (Komplex a2 b2) = (b1 * a2 - a1 * b2) / (a2^2 + b2^2)

        fromRational r = Komplex (fromRational r) 0


