import Data.List (subsequences, permutations)

-- Genera todas las permutaciones de una cadena de caracteres.
allPermutations :: String -> [String]
allPermutations s = concatMap permutations (subsequences s)


combinaciones :: String -> [String]
combinaciones s = concatMap (filter (not . null) . permutations)  (subsequences s)

esVocal :: Char -> Bool
esVocal c = c `elem` "aeiouAEIOU"
esConsonante :: Char -> Bool
esConsonante = not . esVocal

filtrado1 :: String -> [String]
filtrado1 s = filter (\x -> length x > 1 && (esVocal (head x) && esConsonante (last x))) (combinaciones s)

--Incluir el filtrado1 en el filtrado2
filtrado2 :: String -> [String]
filtrado2 s = filter (not . vocalesSeguidas) lista
    where lista = filtrado1 s

vocalesSeguidas :: String -> Bool
vocalesSeguidas [] = False
vocalesSeguidas [_] = False
vocalesSeguidas (x:y:xs) = (esVocal x && esVocal y && x==y) || vocalesSeguidas (y:xs)


filtrado3 :: String -> [String]
filtrado3 s = filter letraFinal lista
    where lista = filtrado2 s

letraFinal :: String -> Bool
letraFinal s | esVocal (last s) = True
             | (last s) `elem` "lnrsLNSR" = True
             | otherwise = False

--filtrado4 :: String -> [String]
--filtrado4 s = filter (not . consonantesSeguidas) lista
--    where lista = filtrado3 s

--consonantesSeguidas :: String -> Bool -- Solo puede haber ll o rr
--consonantesSeguidas [] = False
--consonantesSeguidas [_] = False
--consonantesSeguidas (x:y:xs) = (esConsonante x && esConsonante y && x==y && (x `elem` "lrLR")) || consonantesSeguidas (y:xs)