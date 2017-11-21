isPar :: Int -> Bool
isPar a = if mod a 2 == 0 then True else False

solve :: [Int] -> [Int]
solve [] = []
solve (x:xs) = if isPar x == False then [x] ++ solve xs else solve xs