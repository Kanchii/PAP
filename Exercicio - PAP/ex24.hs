data Tree x = Nil | Node (Tree x) x (Tree x)
    deriving Show

insereTree :: Ord x => x -> Tree x -> Tree x
insereTree x Nil = Node Nil x Nil
insereTree x (Node esq v dir)
  | x == v = Node esq v dir
  | v < x = Node esq v (insereTree x dir)
  | v > x = Node (insereTree x esq) v dir

montaArvore :: Ord x => x -> [x] -> Tree x -> Tree x
montaArvore x [] y = insereTree x y
montaArvore xx (x:xs) y = montaArvore xx xs (insereTree x y)

solve :: Ord x => x -> [x] -> Tree x
solve xx (x:xs) = montaArvore xx (x:xs) Nil
