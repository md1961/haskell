-- Reverse Polish Notation Calculator
calcRPN :: (Num a, Read a) => String -> a
calcRPN = head . foldl foldingFunc [] . words
    where foldingFunc (y:x:xs) "+" = (x + y):xs
          foldingFunc (y:x:xs) "-" = (x - y):xs
          foldingFunc (y:x:xs) "*" = (x * y):xs
          foldingFunc xs x = read x:xs
