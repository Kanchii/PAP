data Tree x = Nil | Node (Tree x) x (Tree x)
    deriving Show

insereTree :: Ord x => x -> Tree x -> Tree x
insereTree x Nil = Node Nil x Nil
insereTree x (Node esq v dir)
  | x == v = Node esq v dir
  | v < x = Node esq v (insereTree x dir)
  | v > x = Node (insereTree x esq) v dir

elementoEsquerda :: Ord x => Tree x -> x
elementoEsquerda (Node Nil v _) = v
elementoEsquerda (Node esq _ _) = elementoEsquerda esq

deletaAux :: Ord x => Tree x -> Tree x
deletaAux (Node Nil v dir) = dir
deletaAux (Node esq v Nil) = esq
deletaAux (Node esq v dir) = Node esq v2 (deleta v2 dir)
          where v2 = elementoEsquerda dir

deleta :: Ord x => x -> Tree x -> Tree x
deleta _ Nil = Nil
deleta x (Node esq v dir)
  | x == v = deletaAux (Node esq v dir)
  | x < v = Node (deleta x esq) v dir
  | x > v = Node esq v (deleta x dir)

montaArvore :: Ord x => x -> [x] -> Tree x -> Tree x
montaArvore x [] y = deleta x y
montaArvore xx (x:xs) y = montaArvore xx xs (insereTree x y)

solve :: Ord x => x -> [x] -> Tree x
solve xx (x:xs) = montaArvore xx (x:xs) Nil
