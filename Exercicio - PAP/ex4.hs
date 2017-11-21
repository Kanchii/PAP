conta 0 _ = []
conta _ [] = []
conta a (x:xs) = [x] ++ conta (a-1) xs
