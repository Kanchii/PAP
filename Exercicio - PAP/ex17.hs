solve :: [Bool] -> Bool
solve (x:xs) = (foldr (&&) True (x:xs))