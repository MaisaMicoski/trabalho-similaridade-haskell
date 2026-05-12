import Data.List (group, sort, sortBy)

-- contador de frequencia basica
contaFreq :: [String] -> [(String, Int)]
contaFreq palavras = 
    let palavrasOrd = sort palavras
        grupos = group palavrasOrd
    in map (\g -> (head g, length g)) grupos

-- ordenador do relatorio
ordenaRel :: [(String, Int)] -> [(String, Int)]
ordenaRel = sortBy regra
  where
    regra (palavra1, freq1) (palavra2, freq2)
      | freq1 == freq2 = compare palavra1 palavra2
      | otherwise      = compare freq2 freq1

-- multiplica por 2 se for reservada
aplicaPeso :: [String] -> [(String, Int)] -> [(String, Int)]
aplicaPeso palavrasRes frequencia = map mult frequencia
  where
    mult (palavra, freq)
      | palavra `elem` palavrasRes = (palavra, freq * 2)
      | otherwise                         = (palavra, freq)

-- funcao principal que junta tudo
frequenciaProc :: [String] -> [String] -> [String] -> [(String, Int)]
frequenciaProc palavras palavraReserv separadores = 
    let palavrasFil = filter (`notElem` separadores) palavras -- remove os separadores
        contagemBasic = contaFreq palavrasFil          -- conta as palavras
        contagemCPeso = aplicaPeso palavraReserv contagemBasic -- aplica peso 2
    in ordenaRel contagemCPeso                             -- ordena e retorna

-- ajusta o texto, substituindo os separadores por espaços para facilitar a contagem
ajustaText :: String -> [String] -> String
ajustaText texto separadores = 
    -- 'concat separadores' transforma ["(", ")", ";"] em "();"
    let caracteresSep = concat separadores 
        substitui c 
            | c `elem` caracteresSep = ' ' -- se for separador, vira espaço
            | otherwise                      = c   -- se não, mantém a letra
    in map substitui texto

--NOVO--
--calcula a soma das frequencias em freq1 com base nas metricas definidas
calculaSoma :: [(String, Int)] -> [(String, Int)] -> Int
calculaSoma freq1 freq2 = 
    sum[f1 | (palavra, f1) <- freq1,
    Just f2 <- [lookup palavra freq2],
    abs (f1-f2)*10 <= f1]

--calcula o valor final do indice de similaridade
calculaM :: [(String, Int)] -> [(String, Int)] -> Float
calculaM freq1 freq2 = 
    let m = calculaSoma freq1 freq2
        somaF1 = sum[ f1 | (_, f1) <- freq1]
    in if somaF1 == 0
        then 0.0
        else fromIntegral m/ fromIntegral somaF1 
    


--ajustaText: remove separadores
--contaFreq: sort- junta as palavras iguais; group- agrupa palavras iguais; map- anota quantas vezes a palavra aparece em cada grupo
--aplicaPeso: aplica a regra das palavras reservadas estabelecidas na atividade
--ordenaRel: ordena do maior pro menor numero, em caso de números iguais usa ordem alfabética
--frequenciaProc: aplica todas as funções anteriores
--main (a mudar): le os txt, faz a limpeza dos textos com ajustaText e roda frequenciaProc 

main :: IO ()
main = do
    putStrLn "etapa de ler os arq"
    
    textoRes <- readFile "res.txt"
    textoSep <- readFile "sep.txt"
    textoC1  <- readFile "c1.txt"
    textoC2  <- readFile "c2.txt"
    
    let reservadas = words textoRes
        separadores = words textoSep
        
        -- 1. Limpamos os textos substituindo separadores por espaços
        textoC1Limpo = ajustaText textoC1 separadores
        textoC2Limpo = ajustaText textoC2 separadores
        
        -- 2. AGORA sim quebramos em palavras (os espaços extras serão ignorados)
        codigo1 = words textoC1Limpo
        codigo2 = words textoC2Limpo
    
    putStrLn "\n--- frequências do arquivo c1.txt ---"
    let freqC1 = frequenciaProc codigo1 reservadas separadores
    print freqC1

    putStrLn "\n--- frequências do arquivo c2.txt ---"
    let freqC2 = frequenciaProc codigo2 reservadas separadores
    print freqC2

    --NOVO--
    putStrLn "\n--- Indice de similaridade ---"
    let m = calculaM freqC1 freqC2
    print m