import Data.List (subsequences, permutations, delete)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (randomRIO)
import Control.Monad (replicateM)






-- Genera todas las permutaciones de una cadena de caracteres.
combinaciones :: String -> [String]
combinaciones s = eliminarDuplicados $ concatMap (filter (not . null) . permutations) (subsequences s)

-- Elimina los elementos duplicados de una lista.
eliminarDuplicados :: Ord a => [a] -> [a]
eliminarDuplicados = Set.toList . Set.fromList

-- Comprueba si un caracter es una vocal.
esVocal :: Char -> Bool
esVocal c = c `elem` "aeiouAEIOU"

-- Comprueba si un caracter es una consonante.
esConsonante :: Char -> Bool
esConsonante = not . esVocal

-- Se queda con las palabras que tengan al menos dos caracteres.
filtroLongitud :: [String] -> [String]
filtroLongitud = filter (\x -> length x > 3)

-- Comprueba si hay dos vocales IGUALES seguidas iguales en una palabra.
vocalesIgualesSeguidas :: String -> Bool
vocalesIgualesSeguidas (x:y:xs) = (esVocal x && esVocal y && x==y) || vocalesIgualesSeguidas (y:xs)
vocalesIgualesSeguidas _ = False

-- Comprueba si hay tres vocales seguidas en una palabra (no necesariamente iguales).
vocalesTresSeguidas :: String -> Bool
vocalesTresSeguidas (x:y:z:xs) = (esVocal x && esVocal y && esVocal z) || vocalesTresSeguidas (y:z:xs)
vocalesTresSeguidas _ = False

-- Tras la letra q siempre tiene que ir la letra u.
letraQ :: String -> Bool
letraQ (x:y:xs) = not (x == 'q' && y /= 'u') && letraQ (y:xs)
letraQ _ = True

-- Tras la letra z siempre tiene que ir a, u, o.
letraZ :: String -> Bool
letraZ (x:y:xs) = not (x== 'z' && ((y/= 'a') ||(y/= 'u') ||(y/= 'o')))
letraZ _ = True

-- Se queda con las palabras que no tengan dos vocales IGUALES seguidas ni tres vocales seguidas.
-- Además, se queda con las palabras que cumplan con las reglas de las letras q y z.
filtroVocales :: [String] -> [String]
filtroVocales = filter (not . vocalesIgualesSeguidas) . filter (not . vocalesTresSeguidas) . filter letraQ . filter letraZ


-- Comprueba si la última letra de una palabra es válida (no puede ser k, v, w, x).
letraFinalPermitida :: String -> Bool
letraFinalPermitida s = last s `notElem` "hkvwxHKVWX"

-- Comprueba si hay tres consonantes seguidas en una palabra.
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


-- Se queda con las palabras cuya última letra sea válida y no termine en dos consonantes seguidas.
filtroFinalPalabra :: [String] -> [String]
filtroFinalPalabra = filter (not . terminaEnDosConsonantes) . filter letraFinalPermitida 


-- Se queda con las palabras que no tengan tres consonantes seguidas.
filtroConsonantes :: [String] -> [String]
filtroConsonantes = filter (not . consonantesSeguidas)


-- Comprueba si una palabra empieza correctamente: con vocal, con dos consonantes permitidas o con consonante y vocal.
empiezaBien :: String -> Bool
empiezaBien (x:y:_) = esVocal x || (esConsonante x && esConsonante y && (x,y) `elem` [('b', 'l'), ('c', 'r'), ('d', 'r'), ('f', 'l'), ('f', 'r'), ('g', 'r'), ('p', 'l'), ('p', 'r'), ('t', 'r'), ('c', 'h'), ('l', 'l')]) || (esConsonante x && esVocal y)
empiezaBien _ = False

-- Filtra las palabras que empiezan con dos consonantes permitidas.
filtroInicioPalabra :: [String] -> [String]
filtroInicioPalabra = filter empiezaBien

-- Comprueba si una palabra sigue las reglas básicas de silabificación.
cumpleSilabificacion :: String -> Bool
cumpleSilabificacion xs = cumpleSilabificacion' (tail xs)
    where cumpleSilabificacion' [] = True
          cumpleSilabificacion' [x] = True
          cumpleSilabificacion' (x:y:ys)
            | esVocal x && esVocal y = cumpleSilabificacion' (y:ys) -- Diptongo o hiato
            | esConsonante x && esConsonante y = (x, y) `elem` [('l', 'l'), ('r', 'r'),('n','c'),('d','r'), ('s','h'),('n','h'),('l','p')] && cumpleSilabificacion' (y:ys)
            | esConsonante x && esVocal y = cumpleSilabificacion' (y:ys)
            | esVocal x && esConsonante y = cumpleSilabificacion' (y:ys)
            | otherwise = False

-- Filtra las palabras que cumplen con las reglas de silabificación.
filtroSilabas :: [String] -> [String]
filtroSilabas = filter cumpleSilabificacion


-- Filtra las palabras según todas las reglas de filtrado.
filtrarPalabras :: String -> [String]
filtrarPalabras = filtroSilabas . filtroInicioPalabra . filtroConsonantes . filtroFinalPalabra . filtroVocales . filtroLongitud . combinaciones


------------------------------------------------------------
-- DICCIONARIO
-- No se si llamarlo filtrado7 por que no se puede meter bien en filtradoPalabras por las monadas 
filtrado7 :: [String] -> IO [String]
filtrado7 lista = do
    diccionario <- cargarDiccionario "dic.txt"
    return $ filter (`Set.member` diccionario) lista


-- Diccionario de palabras válidas.
cargarDiccionario :: String -> IO (Set String)
cargarDiccionario path = do
    contenido <- readFile path
    return $ Set.fromList $ lines contenido

tieneEnie :: String -> Bool
tieneEnie palabra = 'ñ' `elem` palabra

tieneTilde :: String -> Bool
tieneTilde = any (`elem` "áéíóú") 

tieneEspacio :: String -> Bool
tieneEspacio = any (== ' ')

-- Poner vocal normal sin tilde (normalizar).
normalizar :: String -> String
normalizar [] = []
normalizar (x:xs) 
  | x == 'á' = 'a' : normalizar xs
  | x == 'é' = 'e' : normalizar xs
  | x == 'í' = 'i' : normalizar xs
  | x == 'ó' = 'o' : normalizar xs
  | x == 'ú' = 'u' : normalizar xs
  | otherwise = x : normalizar xs

filtraDiccionario :: String -> IO ()
filtraDiccionario path = do
    contenido <- readFile path
    let palabras = lines contenido
    let sinEnie = filter (not . tieneEnie) palabras
    let sinEspacios = filter (not . tieneEspacio) sinEnie
    let normalizarDiccionario = map normalizar sinEspacios
    writeFile "dicFiltrado.txt" (unlines normalizarDiccionario)


imprimeDiccionario :: IO ()
imprimeDiccionario = do
    diccionario <-cargarDiccionario "dicFiltrado.txt"
    let listaDiccionario = Set.toList diccionario
    print listaDiccionario
    print "á"
    putStrLn "á"

------------------------------------------------------------

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

-- Calcular la puntuación de una palabra.
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



--paresCaracteres = [('a', 'e'), ('a', 'o'), ('a', 'i'), ('a', 'u'), ('e', 'o'), ('e', 'i'), ('e', 'u'), ('o', 'i'), ('o', 'u'), ('i', 'u')]

-- Seleccionar una letra aleatoria.
caracterAleatorio :: IO Char
caracterAleatorio = randomRIO ('a', 'z')

-- Seleccionar una vocal aleatoria.
vocalAleatoria :: IO Char
vocalAleatoria = do
    let vocales = ['a', 'e', 'i', 'o', 'u']
    indice <- randomRIO (0, length vocales - 1)
    return (vocales !! indice)

-- Define la bolsa de fichas del juego.
-- 12 a's, 12 e's, 
-- 9 o's
-- 6 i's, 6 s's, r's
-- 5 u's, 5 d's, 5 c's, 5 l's
-- 4 t's,  
-- 3 h's
-- 2 g's, b's, m's, 'p's
-- 1 f, v, y, q, j, ll, rr, x, z
bolsaFichas :: [Char]
bolsaFichas = [ 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a',
                'e', 'e', 'e', 'e', 'e', 'e', 'e', 'e', 'e', 'e', 'e', 'e',
                'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o',
                'i', 'i', 'i', 'i', 'i', 'i',
                's', 's', 's', 's', 's', 's',
                'r', 'r', 'r', 'r', 'r', 'r',
                'u', 'u', 'u', 'u', 'u',
                'd', 'd', 'd', 'd', 'd',
                'c', 'c', 'c', 'c', 'c',
                'l', 'l', 'l', 'l', 'l',
                't', 't', 't', 't',
                'h', 'h', 'h',
                'g', 'g',
                'b', 'b',
                'm', 'm',
                'p', 'p',
                'f','v','y','q','j','x','z']

-- Recibe la primera bolsa de fichas y la mano del jugador. 
-- Devuelve la bolsa de fichas actualizada.
nuevaBolsa :: [Char] -> [Char] -> [Char]
nuevaBolsa xs [] = xs
nuevaBolsa xs (y:ys) = nuevaBolsa (delete y xs) ys



-- Recibe una bolsa de fichas y reparte una mano de 6 letras aleatorias.
repartirMano :: [Char] -> IO [Char]
repartirMano xs = repartirMano' xs 6

repartirMano' :: [Char] -> Int -> IO [Char]
repartirMano' _ 0 = return []
repartirMano' xs n = do
    i <- randomRIO (0, length xs - 1)
    let (ys, zs) = splitAt i xs
    rest <- repartirMano' (ys ++ tail zs) (n - 1)
    return ((xs !! i) : rest)


--repartirMano :: IO String
--repartirMano = do
--    caracteres <- replicateM 4 caracterAleatorio
--    vocales <- replicateM 2 vocalAleatoria
--    return (caracteres ++ vocales)

-- Aumentar la mano con una letra del tablero.
aumentarMano :: String -> Char -> Char -> IO String
aumentarMano mano a b = do

    putStrLn ("Presione 0 para "++ show a ++" y 1 para "++ show b )
    
    eleccion <- getLine
    if eleccion == "0"
        then do
            putStrLn ("Ha seleccionado el carácter " ++ show a)
            return (mano ++ [a])
        else 
            if eleccion == "1" 
            then do
                putStrLn ("Ha seleccionado el carácter " ++ show b)
                return (mano ++ [b])
            else do
                putStrLn "Opción inválida. Intente de nuevo."
                aumentarMano mano a b

-- Comprueba si una lista es un subconjunto de otra.
-- No se usa, lo dejo por si acaso.
esSubconjunto :: String -> String -> Bool
esSubconjunto [] _ = True
esSubconjunto (x:xs) ys = x `elem` ys && esSubconjunto xs (delete x ys)

-- Comprueba que la jugada sea válida, es decir, que la palabra esté en la lista de palabras válidas.
-- Si no es válida, se pide otra jugada.
obtenerJugada :: String -> IO String
obtenerJugada mano = do
    putStrLn "Introduzca su jugada y presione enter para continuar"
    jugada <- getLine
    if jugada `elem` filtrarPalabras mano
        then return jugada
        else do
            putStrLn "Jugada inválida, palabra no existente. Intente de nuevo."
            obtenerJugada mano


-- Resolución de la jugada de un jugador.
-- Se pide una jugada valida, si no es valida se pide otra.
-- Devuelve la puntuación de la jugada.
-- Entrada: mano del jugador con el caracter adicional.
resolucionJugada :: String -> IO Int
resolucionJugada entrada = do
    let lista = filtrarPalabras entrada
    putStrLn "Las palabras válidas que se pueden formar con sus letras son: "
    print lista
    -- Se le pide al jugar que introduzca una jugada.
    putStrLn ("Su nueva mano es: " ++ entrada)
    jugada <- obtenerJugada entrada
    puntuacion <- return $ puntuacion jugada
    putStrLn ("La puntuación de su jugada (palabra) es: " ++ show puntuacion)
    let lista = filtrarPalabras entrada
    --print lista

    putStrLn "Algunas palabras con la máxima puntuación son:"
    -- Limitamos la salida a 5 palabras, para no saturar la consola.
    print $ take 5 $ maximaPuntuacion $ puntuarPalabras lista
    return puntuacion

-- Jugar con un solo jugador.
jugar1Jugador :: IO ()
jugar1Jugador = do
    putStrLn "Se le va a repartir una mano de 6 letras aleatorias. Presione cualquier tecla para continuar."
    _ <- getLine
    entrada <- repartirMano bolsaFichas
    putStrLn ("Su mano es: " ++ entrada)
  
    caracter1 <- vocalAleatoria
    caracter2 <- vocalAleatoria
    putStrLn ("Tienes disponibles estos caracteres: " ++ show caracter1 ++ " y " ++ show caracter2)
    --putStrLn "Presione 0 para el primer carácter y 1 para el segundo carácter"
    nuevaEntrada <- aumentarMano entrada caracter1 caracter2
    --eleccion <- getLine
    --let entradaModificada = if eleccion == "0" then entrada ++ [caracter1] else entrada ++ [caracter2]
    -- resolucion de la jugada
    resolucionJugada nuevaEntrada
    putStrLn "Pulsa q para salir o cualquier otra tecla para volver a jugar."
    opcion <- getLine
    if opcion == "q"
        then putStrLn "¡Gracias por jugar!"
        else main

-- Jugar con dos jugadores.
jugar2Jugadores :: IO ()
jugar2Jugadores = do
    putStrLn "Se van a generar las letras del tablero:"
    caracter1 <- vocalAleatoria
    caracter2 <- vocalAleatoria

    putStrLn "Ahora se repatira la mano al jugador 1"
    entrada1 <- repartirMano bolsaFichas
    -- Actualizar la bolsa de fichas.
    let nuevaBolsaFichas = nuevaBolsa bolsaFichas entrada1
    -- putStrLn ("la nueva bolsa es : " ++ nuevaBolsaFichas)

    putStrLn ("Su mano es: " ++ entrada1)
    nuevaEntrada1 <- aumentarMano entrada1 caracter1 caracter2

    puntuacion1 <- resolucionJugada nuevaEntrada1

    putStrLn "Ahora se repatira la mano al jugador 2"
    entrada2 <- repartirMano nuevaBolsaFichas
    putStrLn ("Su mano es: " ++ entrada2)
    nuevaEntrada2 <- aumentarMano entrada2 caracter1 caracter2

    puntuacion2 <- resolucionJugada nuevaEntrada2

    putStrLn "Puntuación final:"
    putStrLn ("Jugador 1: " ++ show puntuacion1)
    putStrLn ("Jugador 2: " ++ show puntuacion2)
    if puntuacion1 > puntuacion2
        then putStrLn "¡El jugador 1 ha ganado!"
        else if puntuacion2 > puntuacion1
            then putStrLn "¡El jugador 2 ha ganado!"
            else putStrLn "¡Empate!"

    putStrLn "Pulsa q para salir o cualquier otra tecla para volver a jugar."
    opcion <- getLine
    if opcion == "q"
        then putStrLn "¡Gracias por jugar!"
        else main

-- Juego Mini-Scrabble.
jugar :: IO ()
jugar = do
    putStrLn "¿Cuántos jugadores van a jugar?"
    putStrLn "1. Un jugador"
    putStrLn "2. Dos jugadores"
    opcion <- getLine
    if opcion == "1"
        then jugar1Jugador
        else if opcion == "2"
            then jugar2Jugadores
            else do
                putStrLn "Opción inválida. Intente de nuevo."
                jugar

-- Menú de opciones del juego.
opcionesJuego :: IO ()
opcionesJuego = do 
    putStrLn "Al comenzar a jugar se le repartirá una mano de 6 letras aleatorias."
    putStrLn "Y en el tablero se le mostraran 2 posibles letras adicionales."
  
    putStrLn "Cada jugar dispondra de lo siguiente:"
    putStrLn " TU MANO:                TABLERO:"
    putStrLn " __________             __________"
    putStrLn "|  xxxxxx  |           |   y  y   |"
    putStrLn "|__________|           |__________|"

    putStrLn "Pueden jugar hasta 2 jugadores."
    putStrLn "En dicho caso, jugara primero el jugador 1 y luego el jugador 2."

    putStrLn "Para formar una palabra, seleccione una letra de su mano y una del tablero."
    putStrLn "La puntación de la palabra dependerá de las letras seleccionadas."
    putStrLn "Las letras del tablero y de la mano se pueden usar una sola vez."
    putStrLn "Puede que la palabra formada no sea válida, en ese caso, intente de nuevo."
    putStrLn "Pulse cualquier tecla para volver al menú principal. ¡Buena suerte!"
    _ <- getLine
    main

main :: IO ()
main = do
    putStrLn "Bienvenido a Mini-Scrabble"
    putStrLn "¿Que desea hacer?"
    putStrLn "1. Jugar"
    putStrLn "2. Como se juega"
    putStrLn "3. Salir"
    opcion <- getLine
    if opcion == "1" 
        then do
            jugar
        else if opcion == "2"
            then do 
                opcionesJuego
            else if opcion == "3"
                then do
                    putStrLn "¡Gracias por jugar!"
                else do
                    putStrLn "Opción inválida. Intente de nuevo."
                    main
