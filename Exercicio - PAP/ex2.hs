pertence _ [] = False
pertence a (x:xs) = if x == a then True else pertence a xs

intersecao [] [] = []
intersecao [] (y:ys) = []
intersecao (x:xs) [] = []
intersecao (x:xs) (y:ys) = if pertence x (y:ys) == True then [x] ++ intersecao xs (y:ys) else intersecao xs (y:ys)
