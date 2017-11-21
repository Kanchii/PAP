primos :: [Integer]
primos = crivo [2..]

crivo :: [Integer] -> [Integer]
crivo (x:xs) = x : crivo [aux | aux <- xs, aux `mod` x /= 0]