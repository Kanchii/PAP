import Data.List

data Cell = X | O | Empty deriving Show

instance Eq Cell where
    X == O = False
    X == X = True
    X == Empty = False
    O == O = True
    O == X = False
    O == Empty = False
    Empty == X = False
    Empty == O = False
    Empty == Empty = True

type Board = [[Cell]]
type Player = Int
type Pos = (Int, Int)

lineCheck :: Board -> Bool
lineCheck [] = False
lineCheck (x:xs)
    | all (==X) x = True
    | all (==O) x = True
    | otherwise = lineCheck xs

columnCheck :: Board -> Bool
columnCheck board = lineCheck (transpose board)

getFirstDiagonal :: Board -> [Cell]
getFirstDiagonal board = [board!!0!!0, board !! 1 !! 1, board !! 2 !! 2]

getSecondDiagonal :: Board -> [Cell]
getSecondDiagonal board = [board !! 0 !! 2, board !! 1 !! 1, board !! 2 !! 0]

diagonalsCheck :: Board -> Bool
diagonalsCheck board = do
    let fd = getFirstDiagonal board
    let sd = getSecondDiagonal board
    if ((all (== X) fd) || (all (== O) fd) || (all (== X) sd) || (all (== O) sd)) then True else False

verifyBoard :: Board -> Bool
verifyBoard board
	| columnCheck board = True
	| lineCheck board = True
	| diagonalsCheck board = True
	| otherwise = False

verifyDraw :: Board -> Bool
verifyDraw board = foldl (&&) True [if board !! x !! y == Empty then False else True | x <- [0..2], y <- [0..2]]

newBoard :: Board
newBoard = [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]]

printBoard :: Board -> IO()
printBoard [] = putStrLn ""
printBoard (x:xs) = do
    let line = ((if (x!!0) == Empty then "_" else show (x!!0)) ++ " " ++ (if (x!!1) == Empty then "_" else show (x!!1)) ++ " " ++ (if (x!!2) == Empty then "_" else show (x!!2)))
    putStrLn line
    printBoard xs

changeCellInMatrix :: Board -> Pos -> Cell -> Board
changeCellInMatrix board (x, y) value = do
    let pos = 3 * x + y
    changeCellInMatrix' board pos value
        where
            changeCellInMatrix' :: Board -> Int -> Cell -> Board
            changeCellInMatrix' [] _ _ = []
            changeCellInMatrix' (x:xs) pos value = do
            if (pos < 3) then do
                [changeLine x pos value] ++ xs
            else [x] ++ changeCellInMatrix' xs (pos - 3) value
                where
                    changeLine :: [Cell] -> Int -> Cell -> [Cell]
                    changeLine [] _ _ = []
                    changeLine (x:xs) pos value = do
                        if pos == 0 then [value] ++ xs
                        else [x] ++ changeLine xs (pos-1) value

readInt :: IO [Int]
readInt = fmap (map read.words) getLine

valid :: Int -> Int -> Bool
valid x y = if x >= 0 && y >= 0 && x < 3 && y < 3 then True else False

getMaxiPos :: [Int] -> Int -> (Int, Int)
getMaxiPos (x:[]) pos = (x, pos)
getMaxiPos (x:xs) pos = do
    let res = getMaxiPos xs (pos + 1)
    if x > (fst res) then (x, pos) else res

intToPos :: Int -> Pos
intToPos 0 = (0,0)
intToPos 1 = (0,1)
intToPos 2 = (0,2)
intToPos 3 = (1,0)
intToPos 4 = (1,1)
intToPos 5 = (1,2)
intToPos 6 = (2,0)
intToPos 7 = (2,1)
intToPos 8 = (2,2)

minMax :: Board -> IO Board
minMax board = do
    let res = [if board !! x !! y == Empty then (minMax' (changeCellInMatrix board (x, y) X) 0) else -2 | x <- [0..2],  y <- [0..2]]
    let maxPos = intToPos $ snd (getMaxiPos res 0)
    return (changeCellInMatrix board maxPos X)
    where
        minMax' :: Board -> Int -> Int
        minMax' board minOrMax = do
            if verifyBoard board then (if minOrMax == 0 then 1 else -1)
            else if verifyDraw board then 0
            else
                if minOrMax == 0 then do
                    minimum [ minMax' (changeCellInMatrix board (x, y) O) 1 | x <- [0..2], y <- [0..2], board !! x !! y == Empty]
                else
                    maximum [ minMax' (changeCellInMatrix board (x, y) X) 0 | x <- [0..2], y <- [0..2], board !! x !! y == Empty]
play :: Player -> Board -> IO Board
play p board = do
        let jogador = "Jogador: " ++ show p
        putStrLn jogador
        printBoard board
        let posJogada = "Digite a posicao da jogada:"
        putStrLn posJogada
        pos <- readInt
        let x = pos !! 0
        let y = pos !! 1
        if not (valid x y) then do
            putStrLn "Jogada invalida. Tente outra posicao."
            putStrLn ""
            play p board
        else if (board !! x !! y /= Empty) then do
            	putStrLn "Celula ocupada. Tente outra posicao."
            	putStrLn ""
            	play p board
        else return (changeCellInMatrix board (x, y) (if p == 1 then O else X))

game :: Player -> Board -> IO (Player, Board)
game p board = do
    if p == 2 then do
        nextBoard <- minMax board
        if (verifyBoard nextBoard) == True then return (2, nextBoard)
    	else game 1 nextBoard
    else do
        if verifyDraw board then return (0, board)
        else do
            nextBoard <- play p board
            if (verifyBoard nextBoard) == True then return (1, nextBoard)
        	else if (verifyDraw nextBoard) == True then return (0, nextBoard)
        	else game 2 nextBoard


main :: IO()
main = do
    let initBoard = newBoard

    winner <- game 1 initBoard
    printBoard (snd winner)
    if(fst winner == 0) then putStrLn ("Empate!!")
    else if (fst winner == 1) then putStrLn ("Voce venceu!!")
    else putStrLn ("A IA venceu!!")

    putStrLn ("Deseja jogar novamente? (Y/N)")
    res <- getLine
    if (res == "Y" || res == "y") then main
    else return ()
