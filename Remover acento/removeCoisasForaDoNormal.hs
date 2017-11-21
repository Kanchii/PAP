ajusta :: Char -> String -> String -> Char
ajusta e [] _ = e
ajusta e (x:xs) (y:ys)
    | e == x = y
    | otherwise = ajusta e xs ys

removeAcento :: String -> String
removeAcento [] = []
removeAcento (x:xs) = (ajusta x "àâêôûãõáéíóúçüÀÂÊÔÛÃÕÁÉÍÓÚÇÜ" "aaeouaoaeioucuAAEOUAOAEIOUCU") : (removeAcento xs)

isDigit :: Char -> Bool
isDigit a = (if a `elem` ['0'..'9'] then True else False)

isAlpha :: Char -> Bool
isAlpha a = (if (a `elem` ['a'..'z'] || a `elem` ['A'..'Z']) then True else False)

isSpace :: Char -> Bool
isSpace a = (if a == ' ' then True else False)

removeCaracteresBugado :: [Char] -> [Char]
removeCaracteresBugado [] = []
removeCaracteresBugado (x:xs)
    | (isDigit x || isAlpha x || isSpace x) = x : removeCaracteresBugado xs
    | otherwise = removeCaracteresBugado xs

solve :: String -> String
solve a = removeCaracteresBugado (removeAcento a)