solucao _ [] _ = []
solucao a (x:xs) flag = if (x == a && flag == False) then solucao a xs True else [x] ++ solucao a xs flag

solve a (x:xs) = solucao a (x:xs) False
