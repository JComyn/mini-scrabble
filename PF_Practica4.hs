import Data.List (subsequences, permutations)
import Data.Set (Set)
import qualified Data.Set as Set


-- Genera todas las permutaciones de una cadena de caracteres.
combinaciones :: String -> [String]
combinaciones s = eliminarDuplicados $ concatMap (filter (not . null) . permutations) (subsequences s)

eliminarDuplicados :: Ord a => [a] -> [a]
eliminarDuplicados = Set.toList . Set.fromList

-- Comprueba si un caracter es una vocal.
esVocal :: Char -> Bool
esVocal c = c `elem` "aeiouAEIOU"

-- Comprueba si un caracter es una consonante.
esConsonante :: Char -> Bool
esConsonante = not . esVocal

-- Se queda con las palabras que tengan al menos dos caracteres.
filtrado1 :: [String] -> [String]
filtrado1 = filter (\x -> length x > 3)

-- Comprueba si hay dos vocales IGUALES seguidas iguales en una palabra.
vocalesSeguidas :: String -> Bool
vocalesSeguidas (x:y:xs) = (esVocal x && esVocal y && x==y) || vocalesSeguidas (y:xs)
vocalesSeguidas _ = False

-- Comprueba si hay tres vocales seguidas en una palabra (no necesariamente iguales).
vocalesSeguidas2 :: String -> Bool
vocalesSeguidas2 (x:y:z:xs) = (esVocal x && esVocal y && esVocal z) || vocalesSeguidas2 (y:z:xs)
vocalesSeguidas2 _ = False

-- Se queda con las palabras que no tengan dos vocales IGUALES seguidas.
filtrado2 :: [String] -> [String]
filtrado2 = filter (not . vocalesSeguidas) . filter (not . vocalesSeguidas2)

-- Comprueba si la última letra de una palabra es válida (no puede ser k, v, w, x).
letraFinalPermitida :: String -> Bool
letraFinalPermitida s = last s `notElem` "kvwxKVWX"

-- Comprueba si hay dos consonantes seguidas iguales en una palabra (excepto ll y rr).
consonantesSeguidas :: String -> Bool
consonantesSeguidas xs = consonantesSeguidas' (tail xs)
    where
        consonantesSeguidas' (x:y:z:xs) = (esConsonante x && esConsonante y && esConsonante z) || consonantesSeguidas' (y:z:xs)
        consonantesSeguidas' _ = False
        

-- Se queda con los dos últimos caracteres de una palabra.
ultimosDosCaracteres :: String -> String
ultimosDosCaracteres str = drop (length str - 2) str

-- Comprueba si la palabra no termina en dos consonantes seguidas.
terminaEnDosConsonantes :: String -> Bool
terminaEnDosConsonantes [x] = True
terminaEnDosConsonantes s = esConsonante x && esConsonante y
    where [x, y] = ultimosDosCaracteres s


-- Se queda con las palabras cuya última letra sea una vocal o una consonante permitida y no termina en dos consonantes seguidas.
filtrado3 :: [String] -> [String]
filtrado3 = filter (not . terminaEnDosConsonantes) . filter letraFinalPermitida 


-- Se queda con las palabras que no tengan dos consonantes seguidas iguales (excepto ll y rr).
filtrado4 :: [String] -> [String]
filtrado4 = filter (not . consonantesSeguidas)

-- Comprueba si una palabra empieza con dos consonantes IGUALES.
-- TODO:: Hay excepciones de dos consonantes que pueden ir al principio de una palabra. Ejemplo: "blanco".
-- Ver que combinaciones de dos consonantes son válidas.
-- empiezaConDosConsonantes :: String -> Bool
-- empiezaConDosConsonantes (x:y:_) = esConsonante x && esConsonante y && x==y
-- empiezaConDosConsonantes _ = False

-- Comprueba si una palabra empieza bien.
-- Dos consonantes permitidas: bl, cr, dr, fl, fr, gr, pl, pr, tr, tl, ch, ll...
-- Consonante vocal o vocal.

empiezaBien :: String -> Bool
empiezaBien (x:y:_) = esVocal x || (esConsonante x && esConsonante y && (x,y) `elem` [('b', 'l'), ('c', 'r'), ('d', 'r'), ('f', 'l'), ('f', 'r'), ('g', 'r'), ('p', 'l'), ('p', 'r'), ('t', 'r'), ('t', 'l'), ('c', 'h'), ('l', 'l')]) || (esConsonante x && esVocal y)
empiezaBien _ = False

-- Filtra las palabras que empiezan con dos consonantes permitidas.
filtrado5 :: [String] -> [String]
filtrado5 = filter empiezaBien

-- Comprueba si una palabra sigue las reglas básicas de silabificación.
cumpleSilabificacion :: String -> Bool
cumpleSilabificacion xs = cumpleSilabificacion' (tail xs)
    where cumpleSilabificacion' [] = True
          cumpleSilabificacion' [x] = True
          cumpleSilabificacion' (x:y:ys)
            | esVocal x && esVocal y = cumpleSilabificacion' (y:ys) -- Diptongo o hiato
            | esConsonante x && esConsonante y = (x, y) `elem` [('l', 'l'), ('r', 'r'),('n','c'),('d','r')] && cumpleSilabificacion' (y:ys)
            | esConsonante x && esVocal y = cumpleSilabificacion' (y:ys)
            | esVocal x && esConsonante y = cumpleSilabificacion' (y:ys)
            | otherwise = False

-- Filtra las palabras que cumplen con las reglas de silabificación.
filtrado6 :: [String] -> [String]
filtrado6 = filter cumpleSilabificacion

-- Filtra las palabras según todas las reglas de filtrado.
filtrarPalabras :: String -> [String]
filtrarPalabras = filtrado6 . filtrado5 . filtrado4 . filtrado3 . filtrado2 . filtrado1 . combinaciones


{- 
(No usamos ñ)
PUNTUACIÓN DE LAS LETRAS EN EL SCRABBLE
1 punto: A, E, O, S, I, U, N, L, R, T
2 puntos: C, D, G
3 puntos: M, B, P
4 puntos: F, H, V, Y
6 puntos: J
8 puntos: K, Q, W, X
10 puntos: Z
 -}

puntuacion :: String -> Int
puntuacion (x:xs) = puntuacion' (x:xs) 0
    where
        puntuacion' [] n = n
        puntuacion' (x:xs) n
            | x `elem` "aeosiunlrtAEOSIUNLRT" = puntuacion' xs (n+1)
            | x `elem` "cdgCDG" = puntuacion' xs (n+2)
            | x `elem` "mbpMBP" = puntuacion' xs (n+3)
            | x `elem` "fhvyFHVY" = puntuacion' xs (n+4)
            | x `elem` "jJ" = puntuacion' xs (n+6)
            | x `elem` "kqwxKQWX" = puntuacion' xs (n+8)
            | x `elem` "zZ" = puntuacion' xs (n+10)
            | otherwise = puntuacion' xs n
puntuacion _ = 0

-- Obtener una lista de pares (palabra, puntuación) a partir de una lista de palabras.
puntuarPalabras :: [String] -> [(String, Int)]
puntuarPalabras = map (\x -> (x, puntuacion x))

-- Obtener las palabras con la máxima puntuación dada una lista de pares (palabra, puntuación).
maximaPuntuacion :: [(String, Int)] -> [(String, Int)]
maximaPuntuacion xs = filter (\(x, n) -> n == maximaPuntuacion') xs
    where
        maximaPuntuacion' = maximum $ map snd xs


paresCaracteres :: [(Char, Char)]
paresCaracteres = [('a', 'b'), ('c', 'd'), ('e', 'f'), ('g', 'h')]

-- Ejemplo de uso
main :: IO ()
main = do
    putStrLn "Por favor, ingrese una cadena de texto:"
    entrada <- getLine
    --print $ filtrarPalabras entrada
    let indice = length entrada `mod` length paresCaracteres
    let (caracter1, caracter2) = paresCaracteres !! indice
    --let entradaModificada = entrada ++ [caracter1, caracter2]
    putStrLn ("Tienes disponibles estos caracteres: " ++ show caracter1 ++ " y " ++ show caracter2)
    --putStrLn "Presione 0 para el primer carácter y 1 para el segundo carácter"
    --eleccion <- getLine
    --let entradaModificada = if eleccion == "0" then entrada ++ [caracter1] else entrada ++ [caracter2]
    
    let lista = filtrarPalabras entrada
    -- print lista

    print $ maximaPuntuacion $ puntuarPalabras lista
