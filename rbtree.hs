data Cor = V | P
	deriving Show

data RBTree a = Node a Cor (RBTree a) (RBTree a) | Nil
	deriving Show

rot (Node e1 P a (Node e2 V b (Node e3 V c d))) = (Node e2 V (Node e1 P a b) (Node e3 P c d))
rot (Node e3 P (Node e1 V a (Node e2 P b c)) d) = (Node e2 V (Node e1 P a b) (Node e3 P c d))
rot (Node e3 P (Node e2 V (Node e1 V a b) c) d) = (Node e2 V (Node e1 P a b) (Node e3 P c d))
rot (Node e1 P a (Node e3 V (Node e2 P b c) d)) = (Node e2 V (Node e1 P a b) (Node e3 P c d))
rot a = a

ins e a = let(Node e' c esq dir) = ins' e a in (Node e' P esq dir)
	where
		ins' e Nil = Node e V Nil Nil
		ins' e (Node x c esq dir)
			| e == x = rot (Node x c esq dir)
			| e < x = rot (Node x c (ins' e esq) dir)
			| otherwise = rot (Node x c esq (ins' e dir))