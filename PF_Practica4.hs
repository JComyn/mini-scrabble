import Data.List (subsequences, permutations)

-- Genera todas las permutaciones de una cadena de caracteres.
{- allPermutations :: String -> [String]
allPermutations s = concatMap permutations (subsequences s) -}

-- Genera todas las combinaciones de una cadena de caracteres.
combinaciones :: String -> [String]
combinaciones s = concatMap (filter (not . null) . permutations)  (subsequences s)

-- Comprueba si un caracter es una vocal.
esVocal :: Char -> Bool
esVocal c = c `elem` "aeiouAEIOU"

-- Comprueba si un caracter es una consonante.
esConsonante :: Char -> Bool
esConsonante = not . esVocal

-- Se queda con las palabras que tengan al menos dos caracteres.
filtrado1 :: String -> [String]
filtrado1 s = filter (\x -> length x > 1 && (esVocal (head x) && esConsonante (last x))) (combinaciones s)

--Incluir el filtrado1 en el filtrado2
-- Se queda con las palabras que no tengan dos vocales seguidas.
filtrado2 :: String -> [String]
filtrado2 s = filter (not . vocalesSeguidas) lista
    where lista = filtrado1 s

-- Comprueba si hay dos vocales seguidas iguales en una palabra.
vocalesSeguidas :: String -> Bool
vocalesSeguidas [] = False
vocalesSeguidas [_] = False
vocalesSeguidas (x:y:xs) = (esVocal x && esVocal y && x==y) || vocalesSeguidas (y:xs)

-- Se queda con las palarbras cuya última letra sea una vocal o una consonante que pueda ir al final de una palabra.
filtrado3 :: String -> [String]
filtrado3 s = filter letraFinal lista
    where lista = filtrado2 s

-- Comprueba si la última letra de una palabra es una vocal o una consonante que puede ir al final de una palabra.
letraFinal :: String -> Bool
letraFinal s | esVocal (last s) = True
             | last s `elem` "lnrsLNSR" = True
             | otherwise = False

filtrado4 :: String -> [String]
filtrado4 s = filter (not . consonantesSeguidas) lista
    where lista = filtrado3 s

-- Comprueba si hay dos consonantes seguidas iguales en una palabra (excepto ll y rr).
consonantesSeguidas :: String -> Bool -- Solo puede haber ll o rr
consonantesSeguidas [] = False
consonantesSeguidas [_] = False
consonantesSeguidas (x:y:xs) = (esConsonante x && esConsonante y && x==y && (x `notElem` "lrLR")) || consonantesSeguidas (y:xs)