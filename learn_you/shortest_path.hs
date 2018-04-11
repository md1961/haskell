-- [50, 10, 30, 5, 90, 20, 40, 2, 25, 10, 8, 0]

main = do
    contents <- getContents
    let times = map read $ lines contents :: [Int]
        roadSystem = map (\[a,b,c] -> Section a b c) $ splitIntoThrees times
        path = shortestPath roadSystem
        route = concat $ map (show . fst) path
        time = sum $ map snd path
    putStrLn $ "Shortest route is: " ++ route
    putStrLn $ "Consumed time is: " ++ show time

{-
shortestCost :: (Num a, Ord a) => [a] -> a
shortestCost = minimum . foldl foldingFunc [0, 0] . splitIntoThrees
    where   foldingFunc (cumA:cumB:_) (a:b:ab:_) = (min (cumA + a) (cumB + b + ab)):(min (cumA + a + ab) (cumB + b)):[]
-}

splitIntoThrees :: [a] -> [[a]]
splitIntoThrees [] = []
splitIntoThrees [_] = []
splitIntoThrees (_:[_]) = []
splitIntoThrees (x:y:z:zs) = [x,y,z]:splitIntoThrees zs

data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section]

--roadSystem :: RoadSystem
--roadSystem = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data PathLabel = A | B | C deriving (Show)
type Path = [(PathLabel, Int)]

shortestPath :: RoadSystem -> Path
shortestPath = reverse . shorter . foldl foldingFunc ([], [])
    where   foldingFunc path section = (shorter ((A,a):accA, (C,ab):(B,b):accB), shorter ((C,ab):(A,a):accA, (B,b):accB))
                where   accA = fst path
                        accB = snd path
                        a = getA section
                        b = getB section
                        ab = getC section

shorter :: (Path, Path) -> Path
shorter (path1, path2) = if (pathSum path1) <= (pathSum path2) then path1 else path2

pathSum :: Path -> Int
pathSum = foldr (\path acc -> acc + snd path) 0
