import Data.List (sort)
import Data.Word
import qualified Data.Binary.Put as P
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I

data Huffman = Folha Int Char | No Int Huffman Huffman
    deriving Show

--Conta a frequencia de um simbolo em uma string
contaFreq :: Char -> String -> Int
contaFreq _ [] = 0
contaFreq x (y:ys)
    | x == y = 1 + contaFreq x ys
    | otherwise = contaFreq x ys

--Monta um array com dados do tipo Huffman contendo a frequencia de cada caracter na string
freqSimb :: String -> [Huffman]
freqSimb [] = []
freqSimb (x:xs) = (Folha (1 + contaFreq x xs) x) : freqSimb [y | y <- xs, y /= x]

-- Sobrecarregando os operadores do tipo Huffman para usar no sort
instance Eq Huffman where
    (Folha v1 _) == (Folha v2 _) = v1 == v2
    (Folha v1 _) == (No v2 _ _) = v1 == v2
    (No v1 _ _) == (Folha v2 _) = v1 == v2
    (No v1 _ _) == (No v2 _ _) = v1 == v2

instance Ord Huffman where
    compare (Folha v1 _) (Folha v2 _) = compare v1 v2
    compare (Folha v1 _) (No v2 _ _) = compare v1 v2
    compare (No v1 _ _) (Folha v2 _) = compare v1 v2
    compare (No v1 _ _) (No v2 _ _) = compare v1 v2
--------------------------------------------------------------------------

--Insere um No no vetor
insereNo :: Huffman -> [Huffman] -> [Huffman]
insereNo no [] = [no]
insereNo no@(No v1 _ _) (x@(Folha v2 _) : xs) = do
    if v2 > v1 then (no : x : xs) else x : insereNo no xs
insereNo no@(No v1 _ _) (x@(No v2 _ _) : xs) = do
    if v2 > v1 then (no : x : xs) else x : insereNo no xs

--Função que constroi a arvore de Huffman
construirArvore :: [Huffman] -> Huffman
construirArvore [n@(No v l r)] = n
construirArvore (f1@(Folha a b) : f2@(Folha c d) : xs) = construirArvore $ insereNo (No (a + c) f1 f2)  xs
construirArvore (f1@(Folha a b) : n1@(No v1 l1 r1) : xs) = construirArvore $ insereNo (No (a + v1) f1 n1) xs
construirArvore (n1@(No v1 l1 r1) : f1@(Folha a b) : xs) = construirArvore $ insereNo (No (a + v1) n1 f1) xs
construirArvore (n1@(No v1 l1 r1) : n2@(No v2 l2 r2) : xs) = construirArvore $ insereNo (No (v1 + v2) n1 n2) xs

--Adiciona um bit 0 ou 1 a todas as tuplas a linha da árvore
add :: Char -> [(Char, String)] -> [(Char, String)]
add v [] = []
add v ((c, s) : xs) = [(c, v : s)] ++ add v xs

--Função que mapeia cada char do texto para um sequencia de bits
codHuffman :: Huffman -> [(Char, String)]
codHuffman (Folha _ c) = [(c, "")]
codHuffman (No _ l r) = (add '0' $ codHuffman l) ++ (add '1' $ codHuffman r)

--Procura na lista de codigos o nome do caracter em questão
pegaCod :: Char -> [(Char, String)] -> String
pegaCod e ((c, s) : xs)
    | e == c = s
    | otherwise = pegaCod e xs

--Codifica uma palavra
codificar :: String -> [(Char, String)] -> String
codificar [] _ = []
codificar (x:xs) mapeamento = (pegaCod x mapeamento) ++ codificar xs mapeamento

--Decodifica uma palavra
decodificar :: String -> Huffman -> String
decodificar [] _ = []
decodificar _ (Folha v c) = show c
decodificar (x:xs) huff@(No v l r) = do
    if x == '0' then do
        let res = (decodificar' xs l)
        let letra = fst res
        let resto = snd res
        letra : decodificar resto huff
    else do
        let res = (decodificar' xs r)
        let letra = fst res
        let resto = snd res
        letra : decodificar resto huff
    where
        decodificar' :: String -> Huffman -> (Char, String)
        decodificar' xs (Folha v c) = (c, xs)
        decodificar' (x:xs) (No v l r)
            | x == '0' = decodificar' xs l
            | x == '1' = decodificar' xs r

--Conta o numero de caracteres diferentes que tem no texto
numChar :: [Huffman] -> Int
numChar [] = 0
numChar (x:xs) = 1 + numChar xs

--Conta o numero de caracteres que tem no codigo Huffman montado a partir da arvore
quantChar :: String -> Int
quantChar [] = 0
quantChar (x : xs) = 1 + quantChar xs

--Apenas adiciona ao vetor a quantidade de caracteres diferentes e a quantidade de caracteres no binario de Huffman
put' a b = do
    P.putWord8 (toEnum a)
    P.putWord32be (toEnum b)
    P.flush

--Aqui estamos adicionando as tuplas (char, frequencia do char no texto)
put'' [] = P.flush
put'' ((Folha v c) : xs) = do
    P.putWord8 (I.c2w c)
    P.putWord32be (toEnum v)
    put'' xs

--Função para converter uma string em um inteiro (Usado para passar o binario para inteiro)
toInt :: Int -> String -> Int
toInt _ [] = 0
toInt a (x:xs) = (toDigit x) * 2 ^ a + toInt (a+1) xs
    where
        toDigit x = if x == '0' then 0 else 1

--Função que adiciona ao vetor valores correspondentes ao binario separado de 8 em 8
put''' "" = P.flush
put''' xs = do
    let x = toInt 0 (reverse (take 8 xs))
    P.putWord8 (toEnum x)
    put''' (drop 8 xs)

--Função geral para adicionar os valores ao vetor
put freq codificado = do
    put' (numChar freq) (quantChar codificado)
    put'' freq
    put''' codificado

--Main do programa
main :: IO ()
main = do
    --Le o arquivo de entrada
    putStrLn "Digite o nome do arquivo: "
    arq <- getLine
    --Pega o texto do arquivo
    texto <- readFile arq
    --Cria o vetor de frequencias de caracteres
    let freq = freqSimb texto
    --Cria a arvore de Huffman
    let huffTree = construirArvore (sort freq)
    --Mapeia todos os caracteres para binarios
    let charMap = codHuffman huffTree
    --Aqui temos o texto todo codificado em binario usando o mapa criado na linha anterior
    let codificado = codificar texto charMap
    --print(codificado)
    --print (decodificar codificado huffTree)
    --Aqui montamos a estrutura que vai armazanar todos os dados em binario
    let bs = P.runPut (put freq codificado)
    --Escrevemos no arquivo saidaBin.txt o arquivo compactado
    L.writeFile "saidaBin.txt" bs
    return ()

--Função para recuperar uma tupla (Word8, Word32)
getReg = do
    c <- G.getWord8
    f <- G.getWord32be
    return (c,f)

--Função para pegar a primeira tupla do arquivo binario que é a quantidade de caracteres diferentes
--e a quantidade de caracteres no binario do texto codificado
getFirstTupla = do
    c <- G.getWord8
    f <- G.getWord32be
    return (c,f)

--Função para pegar todos as tuplas referentes aos caracteres do texto e suas frequencias
getFreq qtd = do
    if(qtd == 0) then return []
    else do
        k <- getReg
        ks <- getFreq (qtd-1)
        return (k:ks)

--Função para pegar um numero referente ao binario do texto
getBin = do
    x <- G.getWord8
    return x

--Função para pegar todos os inteiros referentes ao binario do texto
getBinarios = do
    empty <- G.isEmpty
    if empty then return []
    else do
        k <- getBin
        ks <- getBinarios
        return (k:ks)

--Função para pegar todos os dados do arquivo binario
getAll = do
    vet@(n,t) <- getFirstTupla
    freq <- getFreq n
    binario <- getBinarios
    return (vet, freq, binario)

--Função para construir um vetor [Huffman] a partir de tuplas (Word8, Word32)
toHuff [] = []
toHuff ((c, v):xs) = do
    (Folha (fromIntegral v) (I.w2c c)) : toHuff xs

--Função para transformar um vetor de algo em uma string
toString [] = []
toString (x:xs) = do
    (I.w2c x) : (toString xs)

--Função para pegar um char e passar para inteiro
toIntFromChar [] = []
toIntFromChar (x:xs) = do
    (fromEnum x) : (toIntFromChar xs)

--Função para preencher uma string com 0 a esquerda até ela ter tamanho 8
completeWithZero :: String -> String
completeWithZero v = do
    if (length v == 8) then v
    else (completeWithZero ('0' : v))

--Função para preencher uma string com 0 a esquerda até ela ter tamanho k
completeWithKZeros :: Int -> String -> String
completeWithKZeros 0 v = v
completeWithKZeros k xs = completeWithKZeros (k-1) ('0' : xs)

--Função para passar vetor de inteiros para uma string em binario
toBin _ [] = []
toBin tam (x:xs) = do
    --Se o x for o ultimo numero do vetor
    if xs == [] then do
        --Passo o numero para binario
        let k = intToBin x
        --Pego o tamanho do numero
        let tamK = length k
        --Se o tamanho do numero for menor do k, então preencha a string com 0 a esquerda até ter tamanho k
        if (tamK < tam) then (completeWithKZeros (tam - tamK) k)
        else k --Caso contrario, apenas retorne a string
    --Se não for o ultimo numero, então transforme para binario, preencha com 0 a esquerda até ter tamanho 8 e va para o proximo numero
    else (completeWithZero (intToBin x)) ++ (toBin (tam - 8) xs)
    --Definição da função para montar o binario de um inteiro
    where
        intToBin 0 = ""
        intToBin a = do
            (intToBin (a `div` 2)) ++ toDigit a
            where
                toDigit a = (if a `mod` 2 == 0 then "0" else "1")

--Função principal para decodificar o arquivo binario
main' :: IO ()
main' = do
    --Abrindo o arquivo
    bs <- L.readFile "saidaBin.txt"
    --Pegando todos os dados contidos no arquivo binario
    let ((c, v), freq, binarios) = G.runGet getAll bs
    --Ordenando a lista de frequencias retirado do arquivo binario
    let sortedList = sort $ toHuff freq
    --Construindo a arvore de Huffman
    let huffTree = construirArvore sortedList
    --Transformando os inteiros referentes ao codigo Huffman contido no arq. binario para string
    let listaBinarioString = toString binarios
    --Passando os dados da lista de cima para inteiros
    let listaBinarioInt = toIntFromChar listaBinarioString
    --Montando o codigo Huffman referente ao texto a ser decodificado
    let textoParaDecodificar = toBin (fromIntegral v) listaBinarioInt
    --Decodificando o texto
    let res = decodificar textoParaDecodificar huffTree
    --Gerando um arquivo de saida com o texto decodificado
    writeFile "textoDecodificado.txt" res
    return ()
