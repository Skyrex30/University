import System.IO
import Control.Monad
import Data.List (sort)

--Count your coins
main :: IO ()
main = do
    valueToReach <- readLn :: IO Int
    n <- readLn :: IO Int
    countsLine <- getLine
    valuesLine <- getLine

    let counts = parseInput countsLine
        values = parseInput valuesLine
        coins = createCoins counts values
        sortedCoins = sort coins
  
    if sum coins < valueToReach
        then print (-1)
        else print (minimumCoins sortedCoins valueToReach)

parseInput :: String -> [Int]
parseInput str = readWords (words str)
  where
    readWords [] = []
    readWords (x:xs) = read x : readWords xs

createCoins :: [Int] -> [Int] -> [Int]
createCoins [] [] = []
createCoins (c:cs) (v:vs) = replicateCoins c v ++ createCoins cs vs
  where
    replicateCoins 0 _ = []
    replicateCoins n x = x : replicateCoins (n - 1) x

minimumCoins :: [Int] -> Int -> Int
minimumCoins coins target = go coins target 0
  where
    go _ t count | t <= 0 = count
    go [] _ _ = -1
    go (c:cs) t count = go cs (t - c) (count + 1)


