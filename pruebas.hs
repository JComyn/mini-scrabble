import Data.List (subsequences, permutations)

-- Genera todas las permutaciones de una cadena de caracteres.
combinaciones :: String -> [String]
combinaciones s = concatMap permutations (subsequences s)

-- Comprueba si un caracter es una vocal.
esVocal :: Char -> Bool
esVocal c = c `elem` "aeiouAEIOU"

-- Comprueba si un caracter es una consonante.
esConsonante :: Char -> Bool
esConsonante = not . esVocal

-- Se queda con las palabras que tengan al menos dos caracteres.
filtrado1 :: [String] -> [String]
filtrado1 = filter (\x -> length x > 1)

-- Comprueba si hay dos vocales seguidas iguales en una palabra.
vocalesSeguidas :: String -> Bool
vocalesSeguidas (x:y:xs) = (esVocal x && esVocal y) || vocalesSeguidas (y:xs)
vocalesSeguidas _ = False

-- Se queda con las palabras que no tengan dos vocales seguidas.
filtrado2 :: [String] -> [String]
filtrado2 = filter (not . vocalesSeguidas)

-- Comprueba si la última letra de una palabra es una vocal o una consonante que puede ir al final de una palabra.
letraFinalPermitida :: Char -> Bool
letraFinalPermitida c = esVocal c || c `elem` "lnrsLNSR"

-- Comprueba si hay dos consonantes seguidas iguales en una palabra (excepto ll y rr).
consonantesSeguidas :: String -> Bool
consonantesSeguidas (x:y:xs) = (esConsonante x && esConsonante y && x == y && x `notElem` "lrLR") || consonantesSeguidas (y:xs)
consonantesSeguidas _ = False

-- Comprueba si la palabra no termina en dos consonantes seguidas.
noTerminaEnDosConsonantes :: String -> Bool
noTerminaEnDosConsonantes [x] = True
noTerminaEnDosConsonantes (x:y:[]) = not (esConsonante x && esConsonante y)
noTerminaEnDosConsonantes _ = True

-- Se queda con las palabras cuya última letra sea una vocal o una consonante permitida y no termina en dos consonantes seguidas.
filtrado3 :: [String] -> [String]
filtrado3 = filter (\x -> letraFinalPermitida (last x) && noTerminaEnDosConsonantes x)

-- Se queda con las palabras que no tengan dos consonantes seguidas iguales (excepto ll y rr).
filtrado4 :: [String] -> [String]
filtrado4 = filter (not . consonantesSeguidas)

-- Comprueba si una palabra empieza con dos consonantes.
-- TODO:: Hay excepciones de dos consonantes que pueden ir al principio de una palabra. Ejemplo: "blanco".
-- Ver que combinaciones de dos consonantes son válidas.
empiezaConDosConsonantes :: String -> Bool
empiezaConDosConsonantes (x:y:_) = esConsonante x && esConsonante y
empiezaConDosConsonantes _ = False

-- Filtra las palabras que empiezan con dos consonantes.
filtrado5 :: [String] -> [String]
filtrado5 = filter (not . empiezaConDosConsonantes)

-- Aplica todos los filtros a las combinaciones generadas, incluyendo el nuevo filtro.
filtrarPalabras :: String -> [String]
filtrarPalabras = filtrado5 . filtrado4 . filtrado3 . filtrado2 . filtrado1 . combinaciones

-- Ejemplo de uso
main :: IO ()
main = do
    let entrada = "lvroeub"
    print $ length $ filtrarPalabras entrada
    -- print $ filtrarPalabras entrada
