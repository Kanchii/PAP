data Tree x = Nil | Node (Tree x) x (Tree x)
    deriving Show

insereTree :: Ord x => x -> Tree x -> Tree x
insereTree x Nil = Node Nil x Nil
insereTree x (Node esq v dir)
  | x == v = Node esq v dir
  | v < x = Node esq v (insereTree x dir)
  | v > x = Node (insereTree x esq) v dir

montaArvore :: Ord x => [x] -> Tree x -> Tree x
montaArvore [] y = y
montaArvore (x:xs) y = montaArvore xs (insereTree x y)

emOrdem :: Ord x => Tree x -> [x]
emOrdem Nil = []
emOrdem (Node esq x dir) = emOrdem esq ++ [x] ++ emOrdem dir

solve :: Ord x => [x] -> [x]
solve (x:xs) = emOrdem (montaArvore (x:xs) Nil)
