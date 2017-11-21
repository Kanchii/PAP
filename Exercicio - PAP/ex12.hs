solucao _ [] _ = []
solucao a (x:xs) True = [x] ++ solucao a xs True
solucao a (x:xs) False = if (x > a) then [a] ++ [x] ++ solucao a xs True else [x] ++ solucao a xs False

solve a (x:xs) = solucao a (x:xs) False
