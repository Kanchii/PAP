solve [] = 10^100
solve (x:xs) = if x <= solve(xs) then x else solve(xs)
