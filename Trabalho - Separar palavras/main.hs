import System.IO
import System.Environment

type Doc = String
type Line = String
type Word' = String

--Método para printar a saida final do programa uma palavra por linha
toString :: [([Int], Word')] -> IO ()
toString [] = return ()
toString (x : xs) = do
    print x
    toString xs

--Transforma uma lista de inteirosal seriao Man tds vivo para string
aux :: [Int] -> String
aux []= []
aux [x] = show x
aux (x:xs) = show x ++ "," ++ (aux xs)

--Passa a saido do makeindex para string
makeString :: [([Int], Word')] -> String
makeString [] = []
makeString ((a, b) : xs) = "([" ++ (aux a) ++ "]," ++ b ++ ")\n" ++ makeString xs

--Main do programa, ela apenas pega o nome do arquivo e chama o método principal (makeindex)
--passando o arquivo bem como ja chama o método (toString) para printar a saida
main = do
    putStrLn "Digite o nome do arquivo: "
    arq <- getLine
    texto <- readFile arq
    let x = makeindex texto
    arq <- openFile "saida" WriteMode
    hPutStr arq (makeString $ x)
    hFlush arq
    hClose arq
    putStrLn "Arquivo de saida gerado com sucesso!"

--Método principal que chama todos os outros metodos usados para resolver o problema
--de separar o texto em linhas e palavras bem como indexar todas
makeindex :: Doc -> [([Int], Word')]
makeindex documento = sortLs $ allNumWords $ numLines $ ajustaTudo $ getLines documento

--Método para retirar acentos e pontuações do texto
ajustaTudo :: [Line] -> [Line]
ajustaTudo [] = []
ajustaTudo (x : xs) = removePontuacao (removeAcento x) : ajustaTudo xs

--Conjunto de métodos para fazer verificações dos caracteres do texto
--Verificam se é um digito, um caracter do alfabeto ou se é um espaço
------------------------------------------------------------------------------------------------
isDigit :: Char -> Bool
isDigit a = (if a `elem` ['0'..'9'] then True else False)

isAlpha :: Char -> Bool
isAlpha a = (if (a `elem` ['a'..'z'] || a `elem` ['A'..'Z']) then True else False)

isSpace :: Char -> Bool
isSpace a = (if a == ' ' then True else False)
------------------------------------------------------------------------------------------------

--Método para remover as pontuações do texto
removePontuacao :: Line -> Line
removePontuacao [] = []
removePontuacao (x:xs)
    | (isDigit x || isAlpha x || isSpace x) = x : removePontuacao xs
    | otherwise = [] ++ removePontuacao xs

--Método para retirar os acentos do texto
removeAcento' :: Char -> String -> String -> Char
removeAcento' e [] _ = e
removeAcento' e (x:xs) (y:ys)
    | e == x = y
    | otherwise = removeAcento' e xs ys

removeAcento :: Line -> Line
removeAcento [] = []
removeAcento (x:xs) = (removeAcento' x "àâêôûãõáéíĩîìçóúüÀÂÊÔÛÃÕÁÉÍÇÓÚÜ" "aaeouaoaeiiiicouuAAEOUAOAEICOUU") : (removeAcento xs)

--Método para "quebrar" o documento em linhas
getLines :: Doc -> [Line]
getLines documento = lines documento

--Método para enumerar todas as linhas
numLines :: [Line] -> [(Int, Line)]
numLines xs = zip [1..] xs

--Método para enumerar cada palavra de acordo com o numero da sua linha
getWords :: Int -> [Word'] -> [(Int, Word')]
getWords _ [] = []
getWords a (x : xs) = (a, x) : (getWords a xs)

--Método para quebrar a linha em palavras e chamar o método acima para enumerar as palavras
allNumWords :: [(Int, Line)] -> [(Int, Word')]
allNumWords [] = []
allNumWords ((numLinha, linha) : xs) = (getWords numLinha (words linha)) ++ allNumWords xs

--Método para remover as repetições de indices iguais para a mesma palavra
--ex: ([1,1,1,2], Abobora) -> A palavra Abobora apareceu 3x na primeira linha e 1x na segunda
--Apos o processamento do método, temos como saida: ([1,2], Abobora) -> A palavra
--Abobora apareceu na linha 1 e 2
removeRep :: [Int] -> [Int]
removeRep [] = []
removeRep (x : xs) = [x] ++ removeRep resto
    where
        resto = [y | y <- xs, y /= x]

--Método que junta todos os indices das palavras em um unico vetor
juntaIndices :: [(Int, Word')] -> [Int]
juntaIndices [] = []
juntaIndices ((num, _) : xs) = removeRep ([num] ++ juntaIndices xs)

--Método para juntar todas as palavras iguais em um unico vetor com a concatenação de seus
--indices em um unico vetor também
juntaPalavras :: [(Int, Word')] -> [([Int], Word')]
juntaPalavras ((a, palavra) : xs) = [(juntaIndices ((a, palavra) : xs), palavra)]

--Método simples que usa o quicksort para ordenar todas as palavras do texto por ordem alfabética
sortLs :: [(Int, Word')] -> [([Int], Word')]
sortLs [] = []
sortLs ((num, palavra) : xs) = sortLs small ++ mid ++ sortLs large
    where
        small = [(x, y) | (x, y) <- xs, y < palavra]
        mid = juntaPalavras ([(num, palavra)] ++ [(x, y) | (x, y) <- xs, y == palavra])
        large = [(x, y) | (x, y) <- xs, y > palavra]
