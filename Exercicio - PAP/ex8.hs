intToChar 0 = "0"
intToChar 1 = "1"

inverte [] = []
inverte (x:xs) = inverte xs ++ [intToChar x]

solve 0 = []
solve a =  solve (a `div` 2) ++ intToChar (a `mod` 2)
