import Data.List (group, sort, sortBy)

-- | 1. Função principal que conta a frequência de uma lista de palavras
contaFrequencia :: [String] -> [(String, Int)]
contaFrequencia palavras = 
    let palavrasOrdenadas = sort palavras     -- Ordena: ["a", "b", "b", "c"]
        grupos = group palavrasOrdenadas      -- Agrupa: [["a"], ["b", "b"], ["c"]]
    in map (\g -> (head g, length g)) grupos  -- Mapeia: [("a", 1), ("b", 2), ("c", 1)]

-- | 2. Função que aplica a regra de negócio do relatório (Ordenação)
ordenaRelatorio :: [(String, Int)] -> [(String, Int)]
ordenaRelatorio = sortBy regraDeOrdenacao
  where
    regraDeOrdenacao (palavra1, freq1) (palavra2, freq2)
      | freq1 == freq2 = compare palavra1 palavra2 -- Se a frequência for igual, ordem alfabética (crescente)
      | otherwise      = compare freq2 freq1       -- Caso contrário, maior frequência primeiro (decrescente)

-- | 3. Função que junta tudo 
frequenciaProcessada :: [String] -> [(String, Int)]
frequenciaProcessada palavras = ordenaRelatorio (contaFrequencia palavras)