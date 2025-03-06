import Data.List
-- take
take' :: Int -> [t] -> [t]
take' k (x:xs)
    | k > 0 = x:take' (k-1) xs
take' _ _ = []

-- drop
drop' :: Int -> [t] -> [t]
drop' k (_:xs)
    | k > 0 = take' (k-1) xs
drop' _ l = l

-- l = (take n l) ++ (drop n l)
-- k<=0 => [] ++ l = l
-- felt. k-ra igaz: l=(take k l) ++ (drop k l)
-- ? k+1 -re
-- (take(k+1) l) ++ (drop(k+1) l)
--     1. eset: l=[] => [] ++ [] igaz
--     2. eset: l=(x:xs)
--         (x:take k xs) ++ (drop k xs) = x:xs = l

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p (x:xs)
    | p x = x:takeWhile' p xs
takeWhile' _ _ = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p (x:xs)
    | p x = dropWhile' p xs
dropWhile' _ xs = xs

-- listabol halmazt O(n^2)
halmazt :: (Eq t) => [t] -> [t]
halmazt [] = []
halmazt (x:xs)
    | x `eleme` xs = halmazt xs
    | otherwise = x:halmazt xs
        where
            eleme:: (Eq a) => a -> [a] -> Bool
            _ `eleme` [] = False
            a `eleme` (x:xs)
                | a == x = True
                | otherwise = a `eleme` xs

-- O (log n)
h2 :: (Ord a) => [a] -> [a]
h2 l = h2_ $ sort l
    where 
        h2_ (x1:x2:xs)
            | x1 == x2 = h2_ (x2:xs)
            | otherwise = x1: h2_ (x2:xs)
        h2_ xs = xs

group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' (x:xs) = gr_ [x] xs
    where
        gr_ (x:xs) (y:ys)
            | x == y = gr_ (y:x:xs) ys
            | otherwise = (x:xs):gr_ [y] ys
        gr_ l _ = [l]

compact :: Eq a => [a] -> [(a, Int)]
compact l = cc_ ll
    where
        ll = group l
        cc_ [] = []
        cc_ (l:ls) = (head l,length l): cc_ ls

compact'' :: Eq a => [a] -> [(a, Int)]
compact'' l = map(\l ->(head l,length l)) ll
    where
        ll = group l

compact' :: Eq a => [a] -> [(a, Int)]
compact' l = map (\l -> (head l, length l)) $ group l

compact_ :: Eq a => [a] -> [(a, Int)]
compact_ = map (\l -> (head l, length l)).group