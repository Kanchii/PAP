data Nat = Zero | Suc Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Suc a) = 1 + nat2int a

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat a = Suc (int2nat(a - 1))

add :: Nat -> Nat -> Nat
add a Zero = a
add a (Suc b) = add (Suc a) b

mul :: Nat -> Nat -> Nat
mul a Zero = Zero
mul a (Suc b) = add (mul a b) a

expo :: Nat -> Nat -> Nat
expo a Zero = Suc Zero
expo a (Suc b) = mul (expo a b) a

adicao :: Int -> Int -> Int
adicao a b = nat2int (add (int2nat a) (int2nat b))

multiplicacao :: Int -> Int -> Int
multiplicacao a b = nat2int (mul (int2nat a) (int2nat b))

exponenciacao :: Int -> Int -> Int
exponenciacao a b = nat2int (expo (int2nat a) (int2nat b))