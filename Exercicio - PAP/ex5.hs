tamLista [] = 0
tamLista (x:xs) = 1 + tamLista xs

conta 0 _ = []
conta _ [] = []
conta a (x:xs) = if tamLista (x:xs) == a then [x] ++ conta (a-1) xs else conta a xs
