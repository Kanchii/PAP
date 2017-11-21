fibo :: [Integer]
fibo = 0 : 1 : solveFibo 0 1

solveFibo :: Integer -> Integer -> [Integer]
solveFibo a b = [a+b] ++ solveFibo b (a + b)

solve :: Int -> [Integer]
solve a = take a fibo

