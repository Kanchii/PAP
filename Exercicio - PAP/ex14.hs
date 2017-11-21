solve :: (a -> Bool) -> [a] -> [a]
solve _ [] = []
solve f (x:xs) = if f x == True then [x] ++ solve f xs else solve f xs