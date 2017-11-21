solve :: [(a, b)] -> [a]
solve (x:xs) = map (fst) (x:xs)