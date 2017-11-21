pertence _ [] = False
pertence a (x:xs) = if x == a then True else pertence a xs

uniao [] [] = []
uniao [] (y:ys) = (y:ys)
uniao (x:xs) [] = (x:xs)
uniao (x:xs) (y:ys) = if pertence x (y:ys) == False then [x] ++ uniao xs (y:ys) else uniao xs (y:ys)
