fibo :: [Integer]
fibo = 0 : 1 : solve 0 1

solve :: Integer -> Integer -> [Integer]
solve a b = [a+b] ++ solve b (a + b)