import Data.Char
import System.IO()
import Data.String
import Data.Word
import qualified Data.ByteString.Internal   as I
import qualified Data.ByteString.Lazy       as L
import qualified Data.Binary.Get            as G
import qualified Data.Binary.Put            as P
pertence _ [] = False
pertence x (y:ys) = if(x==y) then True else pertence x ys

uniao [] y = y
uniao y [] = y
uniao (x:xs) y = if(pertence x y) then uniao xs y else x:(uniao xs y)

inter [] _ = []
inter _ [] = []
inter (x:xs) y = if(pertence x y) then x:(inter xs y) else inter xs y


nultimos _ [] = []
nultimos x (y:ys) = if(length (y:ys) <= x) then y:(nultimos (x-1) ys) else nultimos x ys

reverter [] = []
reverter (x:xs) = reverter xs ++ ([x])

expo n expo = n^expo

binario '0' = 0
binario '1' = 1

binToInt [] = 0
binToInt (numero:corpo) = if(binario numero == 1) then (expo 2 (length corpo)) + binToInt corpo else binToInt corpo

menorLista (x:[]) = x
menorLista (x:xs) = if (y < x) then y else x
 where y = menorLista xs

par x = if(mod x 2 == 0) then True else False
impar x = if(mod x 2 == 0) then False else True

insereLista x [] = [x]
insereLista x (y:ys) = if(x > y) then y:(insereLista x ys) else x:(y:ys)

sort [x] = [x]
sort (x:xs) = insereLista x (sort xs)

vitormewsmarchijonck =
 do
    print "Vitor Mews Marchi Jonck"
    vitormewsmarchijonck


retornapar (x:xs) = [z | z <- (x:xs), mod z 2 == 0]

predicado :: (t -> Bool) -> [t] -> [t]
predicado f x = [z | z <- x, f z]

retornaimpar (x:xs) = predicado impar (x:xs)

primeiro :: (a,b) -> a
primeiro (a,b) = a

alltrue x= foldr (&&) True x


somatudo x = foldl (+) 0 x

listaduplas [] [] = []
listaduplas (x:xs) (y:ys) = (x,y):(listaduplas xs ys)

fibbonaci 0 = 1
fibbonaci 1 = 1
fibbonaci x = fibbonaci (x-1) + fibbonaci (x-2)

data Semana = Dom | Seg | Ter

instance Show Semana where
    show Dom = "Domingo"
    show Seg = "Segunda"
    show Ter = "Terça"

--doc = "Adao Mels eh	 um menino de garuva\nEle adora sua cidade\nEle gosta de tomar McFlurry\nE comer no BK\n Mesmo com seu amigo Vitao Gauerzera dizendo que nao eh saudavel\nIsso eh influencia do amigo Jonckzera"
doc = "Haskell xablauzera\nUm ababla\nDois Tres kuatro"

numLines documento = zip (lines doc) [1..]

tuplaWords (linha, indice) = zip (words linha) [indice,indice..]

--allNumWords x = map tuplaWords x
allNumWords [] = []
allNumWords ((x,y):xs) = (tuplaWords (x,y))++(allNumWords xs)

insereTupla:: ([Char],Int) -> [([Char],Int)] -> [([Char],Int)]
insereTupla x [] = [x]
insereTupla x (y:ys) = if( (map toUpper (fst x) ) > (map toUpper (fst y)) ) then y:(insereTupla x ys) else x:(y:ys)

sortTupla [x] = [x]
sortTupla (x:xs) = insereTupla x (sortTupla xs)

data Huffman = Folha Int Char | No Int Huffman Huffman deriving Show
freqSimb :: String -> [Huffman]
freqSimb [] = []
freqSimb str@(x:xs) = (Folha (length(filter (==x) str)) x):freqSimb (filter (/=x) xs)

getValue (No x _ _) = x
getValue (Folha x _) = x

insereHuffman x [] = [x]
insereHuffman x (y:ys) = if(getValue x > getValue y) then y:(insereHuffman x ys) else x:(y:ys)

sortHuffman [x] = [x]
sortHuffman (x:xs) = insereHuffman x (sortHuffman xs)

construirArvore:: [Huffman] -> Huffman
construirArvore (x:[]) = x
construirArvore (x:y:ys) = construirArvore (insereHuffman ( No ( getValue x + getValue y ) x y ) ys )


codHuffman':: Huffman -> [Char] -> [(Char, [Char])]
codHuffman' (Folha x c) cod = (c,cod):[]
codHuffman' (No x e d) cod = (codHuffman' e (cod++"0"))++(codHuffman' d (cod++"1"))


codHuffman:: Huffman -> [(Char, [Char])]
codHuffman x = codHuffman' x ""

codificar:: String -> Huffman -> String
codificar x h = codificar' x (codHuffman h)

findCod:: Char -> [(Char, [Char])] -> String
findCod c [] = ""
findCod c ((x,s):ys) = if(x == c) then s else findCod c ys

codificar':: String -> [(Char, [Char])] -> String
codificar' [x] y = (findCod x y)
codificar' (x:xs) y = (findCod x y)++(codificar' xs y)

decodificar:: String -> Huffman -> String
decodificar s h = decodificar' s h "" h

--decodificar':: String -> Huffman -> String -> Huffman -> String
decodificar' xs (Folha x c) palavra arvore = decodificar' xs arvore (palavra++[c]) arvore
decodificar' [] _ palavra _ = palavra
decodificar' (x:xs) (No _ esq dir) palavra arvore = if(x == '1') then decodificar' xs dir palavra arvore else decodificar' xs esq palavra arvore
stringToCod :: String -> String
stringToCod entrada = do
    let x = codificar entrada (construirArvore' entrada)
    if(length x) `mod` 8 == 0 then x else completarZero x (8 - (length x) `mod` 8)

stringToCod' entrada = codificar entrada (construirArvore' entrada)

stringToTuple entrada = codHuffman (construirArvore' entrada)

construirArvore' entrada = construirArvore (sortHuffman (freqSimb entrada))

construirHuffman entrada = decodificar (codificar entrada arvore) arvore
    where arvore = (construirArvore' entrada)

completarZero :: String -> Int -> String
completarZero cod 0 = cod
completarZero cod z = completarZero (cod++"0") (z-1)

createTuplesFreq [] = []
createTuplesFreq ( (Folha x y) :xs) = (y,x): createTuplesFreq xs

putseg x = do
    P.putWord32be (toEnum x)
    P.flush

put' x  = do
    P.putWord8 (toEnum x)
    P.flush

put'' [] = P.flush
put'' ((c,f):xs) = do
    P.putWord8 (I.c2w c)
    P.putWord32be (toEnum f)
    put'' xs

put''' [] = P.flush
put''' xs = do
    P.putWord8(toEnum $ binToInt $ take 8 xs)
    put''' $ drop 8 xs

put entrada = do
  put' (length (createTuplesFreq (freqSimb entrada)))
  putseg (length (stringToCod' entrada))
  put'' (createTuplesFreq (freqSimb entrada))
  put''' (stringToCod entrada)
  --put''' (stringToCod (if (length entrada) `mod` 8 == 0 then entrada else completarZero entrada (8-(length entrada) `mod` 8) ) )

stringToCodOffset entrada = do
    return entrada
    --if entrada `mod` 8 == 0 then return entrada
    --else return (completarZero entrada (8 - ((length entrada) `mod` 8)))

getReg = do
    c <- G.getWord8
    f <- G.getWord32be
    return (c,f)

getReg8 = do
    c <- G.getWord8
    return c

getRegS = do
    empty <- G.isEmpty
    if empty then return []
    else do
        r<-getReg
        rs<-getRegS
        return (r:rs)

getNumTuplas = do
    f <- G.getWord32be
    let x = f;
    return x

getDupla = do
    n <- G.getWord8
    t <- G.getWord32be
    let x = (n,t)
    return x

getFreq x = do
    if x == 0 then return []
    else do
        y    <- getReg
        ys   <- getFreq (x-1)
        return (y:ys)

getByte = do
    empty <- G.isEmpty
    if empty then return []
    else do
        x  <- getReg8
        xs <- getByte
        return (x:xs)

getall = do
    (n,t) <- getDupla
    fs    <- getFreq n
    byte  <- getByte
    return ((n,t),fs,byte)


printRegS [] = return ()
printRegS (r:rs) = do
        printReg r
        printRegS rs

printReg (c,f) = putStrLn(show (I.w2c c) ++ "-" ++ show f)

freqSimbEntrada [] = []
freqSimbEntrada ((c,f):xs) = (Folha (fromIntegral f) (I.w2c c)):(freqSimbEntrada xs)

bsToString [] = []
bsToString (x:xs) = (I.w2c x):(bsToString xs)

printResS [] = return()
printResS (x:xs) = do
    printRes x
    printResS xs

printRes x  = putStrLn(show x)

stringToInt [] = []
stringToInt (x:xs) = (fromEnum x):(stringToInt xs)

intToBin 0 = ""
intToBin n | mod n 2 == 1 = intToBin(n `div` 2) ++ "1"
           | mod n 2 == 0 = intToBin(n `div` 2) ++ "0"

completarZeroInverso entrada 0 = entrada
completarZeroInverso entrada x = "0"++(completarZeroInverso entrada (x-1))

intToString [] = []
intToString (x:xs) = do
    let y = (intToBin x)
    let leny = length y
    let z = if (leny `mod` 8 == 0 && leny > 0) then y else completarZeroInverso y (8-leny `mod` 8)
    return z++(intToString xs)

vStringToString [] = ""
vStringToString (x:xs) = x++(vStringToString xs)

main = do
    putStrLn "Digite o nome do arquivo de entrada"
    arquivoEntrada <- getLine
    arquivo <- readFile arquivoEntrada
    putStrLn (arquivo)
    let bs = P.runPut (put arquivo)
    L.writeFile "Huffman.txt" bs

main' = do
    putStrLn "Digite o nome do arquivo de entrada"
    arquivoEntrada <- getLine
    bs <- L.readFile arquivoEntrada
    let numTuplas = getNumTuplas
    -- let rs = G.runGet getRegS bs
    let rs@((n,t),fs,byte) = G.runGet getall bs
    let frequenciaTuplas = freqSimbEntrada fs
    let arvore = construirArvore (sortHuffman frequenciaTuplas)
    let stringDec = bsToString byte
    let stringDecInt = stringToInt stringDec
    let stringDecodificar = take (fromEnum t) (vStringToString (intToString stringDecInt))
    printRes "T vale : "
    --printRes (show t)
    printRes "N vale : "
    --printRes (show n)
    printRes "Tuplas : "
    --printRegS fs
    printRes "Inteiros : "
    --printResS stringDecInt
    printRes "String crua: "
    --printRes (show stringDec)
    printRes "String para decodificar : "
    --printRes stringDecodificar
    let res = decodificar stringDecodificar arvore
    --printRes res
    writeFile "saidaTeste.txt" res
