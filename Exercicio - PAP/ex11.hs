menor [] = 10^100
menor (x:xs) = if x <= menor(xs) then x else menor(xs)

retira _ [] _ = []
retira a (x:xs) flag = if (x == a && flag == False) then retira a xs True else [x] ++ retira a xs flag

solve [] = []
solve (x:xs) = [menor (x:xs)] ++ solve (retira (menor (x:xs)) (x:xs) False)
