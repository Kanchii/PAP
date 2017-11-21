solve :: [a] -> [b] -> [(a, b)]
solve _ [] = []
solve [] _ = []
solve (x:xs) (y:ys) = [(x, y)] ++ solve xs ys