import Control.Monad.State
import Data.List (sort, nub)
import qualified Data.Map as Map
import Text.Printf

type Memo = Map.Map Int [Int]

lookupMemo :: Int -> Memo -> [Int]
lookupMemo n memo = case Map.lookup n memo of
    Nothing -> [1]
    (Just x) -> x

maxNumInMemo :: Memo -> Int
maxNumInMemo memo = case Map.toList memo of
    [] -> 0
    x -> maximum $ map fst x

products :: State Memo [Int]
products = do
    memo <- get
    let n = maxNumInMemo memo + 1
        result = nub $ sort [x * y | x <- [1 .. n], y <- lookupMemo (n - x) memo]
    put $ Map.insert n result memo
    return result

runProducts :: Int -> Memo -> ([Int], Memo)
runProducts 1 memo = runState products memo
runProducts n memo = runState products newMemo
    where (_, newMemo) = runProducts (n - 1) memo

range :: [Int] -> Int
range [] = error "No range for empty list"
range xs = last xs - head xs

average :: [Int] -> Float
average [] = error "No average for empty list"
average xs = total / size
    where total = fromIntegral $ sum xs
          size = fromIntegral $ length xs

median :: [Int] -> Float
median [] = error "No median for empty list"
median xs
    | odd size = fromIntegral $ xs !! mid
    | otherwise = (fromIntegral sumMid) / 2
        where size = length xs
              mid = size `div` 2
              sumMid = xs !! mid + xs !! (mid - 1)

part :: Int -> String
part n = printf "Range: %d Average: %.2f Median: %.2f" r avg med
    where ps = fst $ runProducts n Map.empty
          r = range ps
          avg = average ps
          med = median ps

main = do
    putStrLn $ part 40
