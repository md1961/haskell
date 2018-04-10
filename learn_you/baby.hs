import Data.Char
import qualified Data.Map as Map

{-

length' :: [a] -> Int
length' [] = 0
length' (x:y) = 1 + length' y

reduce :: (a -> a -> a) -> a -> [a] -> a
reduce _ x [] = x
reduce f x (y:ys) = reduce f (f x y) ys

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = (if f x then [x] else []) ++ filter' f xs

-- find the largest number under 100,000 that's divisible by 3829
largestDivisibleBy3829 :: (Integral a) => a
largestDivisibleBy3829 = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd  n = n : chain (n * 3 + 1)

encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+shift) ords
    in map chr shifted

phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key xs = foldr (\(k,v) acc -> if k == key then Just v else acc) Nothing xs


lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

data LockerState = Free | Taken deriving (Eq, Show)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup num lockerMap =
    case Map.lookup num lockerMap of
        Nothing -> Left $ "Locker number " ++ show num ++ " doesn't exist!"
        Just (state, code) -> if state == Free then Right $ code
                              else Left $ "Locker " ++ show num ++ " is already taken!"


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node z left right)
    | x == z = Node z left right
    | x <  z = Node z (treeInsert x left) right
    | x >  z = Node z left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node z left right)
    | x == z = True
    | x <  z = treeElem x left
    | x >  z = treeElem x right

-}


class YesNo a where
    yesno :: a -> Bool

instance YesNo [a] where
    yesno [] = False
    yesno _ = True
