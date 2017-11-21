inverte [] = []
inverte (x:xs) = inverte xs ++ [charToInt x]

expo _ 0 = 1
expo a b = a * expo a (b-1)

charToInt '0' = 0
charToInt '1' = 1

solve (x:xs) = converte (inverte (x:xs)) 0

converte [] _ = 0
converte (x:xs) b = x * expo 2 b + converte xs (b+1)
