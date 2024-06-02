import Data.List (subsequences, permutations, delete)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Text.Printf (printf)



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

-- Se queda con las palabras que tengan al menos cuatro caracteres.
filtroLongitud :: [String] -> [String]
filtroLongitud = filter (\x -> length x > 3)


-- Comprueba si hay dos vocales IGUALES seguidas iguales en una palabra.
-- Se tienen en cuenta algunas de las excepciones más comunes: leer, zoo, cooperar, poseer
vocalesIgualesSeguidas :: String -> Bool
vocalesIgualesSeguidas "leer" = False
vocalesIgualesSeguidas "zoo" = False
vocalesIgualesSeguidas "cooperar" = False
vocalesIgualesSeguidas "poseer" = False
vocalesIgualesSeguidas (x:y:xs) = (esVocal x && esVocal y && x==y) || vocalesIgualesSeguidas (y:xs)
vocalesIgualesSeguidas _ = False

-- Comprueba si hay tres vocales seguidas en una palabra (no necesariamente iguales).
-- Las posibles excepciones suelen ser conjugaciones de verbos, que no suelen ser válidas en el Scrabble.
vocalesTresSeguidas :: String -> Bool
vocalesTresSeguidas (x:y:z:xs) = (esVocal x && esVocal y && esVocal z) || vocalesTresSeguidas (y:z:xs)
vocalesTresSeguidas _ = False

-- Se queda con las palabras que no tengan dos vocales IGUALES seguidas ni tres vocales seguidas.
-- Además, se queda con las palabras que cumplan con las reglas de las letras q y z.
filtroVocales :: [String] -> [String]
filtroVocales = filter (not . vocalesIgualesSeguidas) . filter (not . vocalesTresSeguidas)


-- Tras la letra q siempre tiene que ir la letra u.
letraQ :: String -> Bool
letraQ (x:y:xs) = not (x == 'q' && y /= 'u') && letraQ (y:xs)
letraQ _ = True

-- Tras la letra z siempre tiene que ir a, u, o.
letraZ :: String -> Bool
letraZ (x:y:xs) = not (x == 'z' && (y /= 'a' && y /= 'u' && y /= 'o'))
letraZ _ = True

-- Antes de una P, no puede haber una N (es siempre MP).
letrasMP :: String -> Bool
letrasMP (x:y:xs) = not (x == 'n' && y == 'p') && letrasMP (y:xs)
letrasMP _ = True

-- Antes de una B, no puede haber una N (es siempre MB).
letrasMB :: String -> Bool
letrasMB (x:y:xs) = not (x == 'n' && y == 'b') && letrasMB (y:xs)
letrasMB _ = True

-- Tras una N/D, no puede haber B (siempre es NV/DV)
letrasNV :: String -> Bool
letrasNV (x:y:xs) = not ((x == 'n' || x == 'd') && y == 'b') && letrasNV (y:xs)
letrasNV _ = True

-- Después de las sílabas iniciales ha-, he-, hi-, hu- debe ir B (no puede ir V).
-- Por ejemplo: hábil, hebilla, híbrido, hubo
silabasConH :: String -> Bool
silabasConH (x:y:z:xs) = not (x == 'h' && (y == 'a' || y == 'e' || y == 'i' || y == 'u') && z == 'v') 
silabasConH _ = True

-- Después de las sílabas iniciales ra-, ro-, ru- debe ir B (no puede ir V).
-- Por ejemplo: rábano, roble, rubio
silabasConR :: String -> Bool
silabasConR (x:y:z:xs) = not (x == 'r' && (y == 'a' || y == 'o' || y == 'u') && z == 'v') 
silabasConR _ = True

-- Filtra según las reglas ortográficas definidas anteriormente.
filtroOrtografia :: [String] -> [String]
filtroOrtografia = filter letraQ . filter letraZ . filter letrasMP . filter letrasMB . filter letrasNV . filter silabasConH . filter silabasConR


-- Comprueba si hay tres consonantes seguidas en una palabra.
consonantesSeguidas :: String -> Bool
consonantesSeguidas xs = consonantesSeguidas' (tail xs)
    where
        consonantesSeguidas' (x:y:z:xs) = (esConsonante x && esConsonante y && esConsonante z) || consonantesSeguidas' (y:z:xs)
        consonantesSeguidas' _ = False
        
-- Se queda con las palabras que no tengan tres consonantes seguidas.
filtroConsonantes :: [String] -> [String]
filtroConsonantes = filter (not . consonantesSeguidas)


-- Comprueba si la última letra de una palabra es válida (no puede ser k, v, w, x).
letraFinalPermitida :: String -> Bool
letraFinalPermitida s = last s `notElem` "hkvwxHKVWX"

-- Se queda con los dos últimos caracteres de una palabra.
ultimosDosCaracteres :: String -> String
ultimosDosCaracteres str = drop (length str - 2) str

-- Comprueba si la palabra no termina en dos consonantes seguidas.
-- Se tienen en cuenta excepciones comunes como: iceberg, test, vals, biceps.
terminaEnDosConsonantes :: String -> Bool
terminaEnDosConsonantes "iceberg" = False
terminaEnDosConsonantes "test" = False
terminaEnDosConsonantes "vals" = False
terminaEnDosConsonantes "biceps" = False
terminaEnDosConsonantes [x] = True
terminaEnDosConsonantes s = esConsonante x && esConsonante y
    where [x, y] = ultimosDosCaracteres s

-- Se queda con las palabras cuya última letra sea válida y no termine en dos consonantes seguidas.
filtroFinalPalabra :: [String] -> [String]
filtroFinalPalabra = filter (not . terminaEnDosConsonantes) . filter letraFinalPermitida 


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
filtrarPalabras = filtroSilabas . filtroInicioPalabra . filtroConsonantes . filtroFinalPalabra . filtroOrtografia . filtroVocales . filtroLongitud . combinaciones


------------------------------------------------------------
-- DICCIONARIO
------------------------------------------------------------
-- El diccionario se encuentra en el archivo "dicFiltrado.txt".
-- Se utilizará para comprobar cuantas de las palabras filtradas existen realmente.

-- Convierte el archivo en una lista de palabras.
-- Se llamara con el diccionario normalizado.
cargarDiccionario :: String -> IO (Set String)
cargarDiccionario path = do
    contenido <- readFile path
    return $ Set.fromList $ lines contenido

tieneEnie :: String -> Bool
tieneEnie palabra = 'ñ' `elem` palabra

tieneEspacio :: String -> Bool
tieneEspacio = elem ' '

-- Poner vocal normal sin tilde (normalizar).
normalizar :: String -> String
normalizar [] = []
normalizar (x:xs) 
  | x == 'á' = 'a' : normalizar xs
  | x == 'é' = 'e' : normalizar xs
  | x == 'í' = 'i' : normalizar xs
  | x == 'ó' = 'o' : normalizar xs
  | x == 'ú' = 'u' : normalizar xs
  | x == 'A' = 'a' : normalizar xs
  | x == 'B' = 'b' : normalizar xs
  | x == 'C' = 'c' : normalizar xs
  | x == 'D' = 'd' : normalizar xs
  | x == 'E' = 'e' : normalizar xs
  | x == 'F' = 'f' : normalizar xs
  | x == 'G' = 'g' : normalizar xs
  | x == 'H' = 'h' : normalizar xs
  | x == 'I' = 'i' : normalizar xs
  | x == 'J' = 'j' : normalizar xs
  | x == 'K' = 'k' : normalizar xs
  | x == 'L' = 'l' : normalizar xs
  | x == 'M' = 'm' : normalizar xs
  | x == 'N' = 'n' : normalizar xs
  | x == 'O' = 'o' : normalizar xs
  | x == 'P' = 'p' : normalizar xs
  | x == 'Q' = 'q' : normalizar xs
  | x == 'R' = 'r' : normalizar xs
  | x == 'S' = 's' : normalizar xs
  | x == 'T' = 't' : normalizar xs
  | x == 'U' = 'u' : normalizar xs
  | x == 'V' = 'v' : normalizar xs
  | x == 'W' = 'w' : normalizar xs
  | x == 'X' = 'x' : normalizar xs
  | x == 'Y' = 'y' : normalizar xs
  | x == 'Z' = 'z' : normalizar xs
  | otherwise = x : normalizar xs

-- Aplica las funciones anteriores para normalizar el diccionario:
-- 1. Elimina las palabras con ñ.
-- 2. Elimina las palabras con espacios.
-- 3. Normaliza las palabras.
-- 4. Escribe el diccionario normalizado en un archivo.
normalizaDiccionario :: String -> IO ()
normalizaDiccionario path = do
    contenido <- readFile path
    let palabras = lines contenido
    let sinEnie = filter (not . tieneEnie) palabras
    let sinEspacios = filter (not . tieneEspacio) sinEnie
    let diccNormalizado = map normalizar sinEspacios
    writeFile "dicFiltrado.txt" (unlines diccNormalizado)


imprimeDiccionario :: IO ()
imprimeDiccionario = do
    diccionario <-cargarDiccionario "dicFiltrado.txt"
    let listaDiccionario = Set.toList diccionario
    print listaDiccionario

-- Filtra las palabras del diccionario que estén en la lista de palabras válidas.
filtraDiccionario :: [String] -> IO [String]
filtraDiccionario lista = do
    dicc <- cargarDiccionario "dicFiltrado.txt"
    return $ filter (`Set.member` dicc) lista


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
    let lista = filtrarPalabras mano
    -- palabrasDiccionario <- filtraDiccionario lista
    if jugada `elem` lista
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
    palabrasDiccionario <- filtraDiccionario lista
    putStrLn "Las palabras válidas que se pueden formar con sus letras son: "
    print lista
    putStrLn ("Un total de " ++ show (length lista) ++ " palabras")
    putStrLn ("De estas palabras, en el diccionario solo hay " ++ show (length palabrasDiccionario))
    putStrLn ("Es decir, el porcentaje de palabras filtradas que realmente existen es del " ++ printf "%.2f" ((fromIntegral (length palabrasDiccionario) * 100 / fromIntegral (length lista)) :: Double) ++ "%")
    putStrLn ""

    -- Se le pide al jugar que introduzca una jugada.
    putStrLn ("Su nueva mano es: " ++ entrada)
    jugada <- obtenerJugada entrada
    puntuacion <- return $ puntuacion jugada
    putStrLn ("La puntuación de su jugada (palabra) es: " ++ show puntuacion)
    

    putStrLn "Algunas palabras con la máxima puntuación son:"
    -- Limitamos la salida a 5 palabras, para no saturar la consola.
    print $ take 5 $ maximaPuntuacion $ puntuarPalabras lista
    return puntuacion


jugar1Jugador :: Integer -> Integer -> IO Integer
jugar1Jugador 0 puntuacion = return puntuacion
jugar1Jugador rondas puntuacion= do
    putStrLn "Se le va a repartir una mano de 6 letras aleatorias. Presione cualquier tecla para continuar."
    _ <- getLine
    entrada <- repartirMano bolsaFichas
    putStrLn ("Su mano es: " ++ entrada)
  
    caracter1 <- vocalAleatoria
    caracter2 <- vocalAleatoria
    putStrLn ("Tienes disponibles estos caracteres: " ++ show caracter1 ++ " y " ++ show caracter2)
    
    nuevaEntrada <- aumentarMano entrada caracter1 caracter2
 
    puntos <- resolucionJugada nuevaEntrada
    jugar1Jugador (rondas-1) (puntuacion + toInteger puntos)
     

-- Juego de 2 jugadores.
-- n = 0 -> 1 ronda (Modo Clasico)
-- n = 3 -> 3 rondas (Modo Rondas)
jugar2JugadoresRondas :: Integer ->  IO ()
jugar2JugadoresRondas n = do
    putStrLn ("Has seleccionado el modo 1v1. Se jugarán " ++ show n ++ " rondas. ¡Buena suerte!")

    putStrLn "Ahora jugara el jugador 1"
    puntuacion1 <- jugar1Jugador n 0
    
    putStrLn "Ahora se repatira la mano al jugador 2"
    puntuacion2 <- jugar1Jugador n 0

    putStrLn "Puntuación final:"
    putStrLn ("Jugador 1: " ++ show puntuacion1)
    putStrLn ("Jugador 2: " ++ show puntuacion2)
    if puntuacion1 > puntuacion2
        then putStrLn "¡El jugador 1 ha ganado!"
        else if puntuacion2 > puntuacion1
            then putStrLn "¡El jugador 2 ha ganado!"
            else putStrLn "¡Empate!"
    putStrLn ""

    putStrLn "Pulsa q para salir o cualquier otra tecla para volver a jugar."
    opcion <- getLine
    if opcion == "q"
        then putStrLn "¡Gracias por jugar!"
        else main


-- Menu de opciones para el juego de 2 jugadores.
unoContraUno :: IO ()
unoContraUno = do
    putStrLn "¿Que desea hacer?"
    putStrLn "1. Modo Clásico (1 ronda)"
    putStrLn "2. Modo Rondas (3 rondas)"
    putStrLn "3. Salir"
    opcion <- getLine
    if opcion == "1" 
        then do
            jugar2JugadoresRondas 1
        else if opcion == "2"
            then do 
                 jugar2JugadoresRondas 3
            else if opcion == "3"
                then do
                    putStrLn "¡Gracias por jugar!"
                else do
                    putStrLn "Opción inválida. Intente de nuevo."
                    unoContraUno


-- Jugar con un solo jugador.
modoPractica :: IO ()
modoPractica = do
    jugar1Jugador 1 0
    putStrLn "Pulsa q para salir o cualquier otra tecla para volver a jugar."
    opcion <- getLine
    if opcion == "q"
        then putStrLn "¡Gracias por jugar!"
        else main


-- Juego Mini-Scrabble.
jugar :: IO ()
jugar = do
    putStrLn "¿Qué quieres hacer?"
    putStrLn "1. Modo Práctica"
    putStrLn "2. 1v1"
    opcion <- getLine
    if opcion == "1"
        then modoPractica
        else if opcion == "2"
            then unoContraUno
            else do
                putStrLn "Opción inválida. Intente de nuevo."
                jugar

-- Menú de opciones del juego.
opcionesJuego :: IO ()
opcionesJuego = do 
    putStrLn ""
    putStrLn "Al comenzar a jugar se le repartirá una mano de 6 letras aleatorias."
    putStrLn "Y en el tablero se le mostraran 2 posibles letras adicionales."
  
    putStrLn "Cada jugar dispondra de lo siguiente:"
    putStrLn " TU MANO:                TABLERO:"
    putStrLn " __________             __________"
    putStrLn "|  xxxxxx  |           |   y  y   |"
    putStrLn "|__________|           |__________|"

    putStrLn "Los modos de juego son los siguientes:"
    putStrLn "          1. Modo Práctica: Juega solo y forma palabras con las letras de tu mano."
    putStrLn "          2. 1v1: Juega contra otro jugador. "
    putStrLn "           ->  2.1 Modo Clásico: Se juega una ronda."
    putStrLn "           ->  2.2 Modo Rondas: Se juegan 3 rondas"
    putStrLn "En el caso de 2. jugara primero el jugador 1 y luego el jugador 2."
    putStrLn "El jugador con mayor puntuación al final de las 3 rondas gana."

    putStrLn "Para formar una palabra, seleccione una letra de su mano y una del tablero."
    putStrLn "La puntación de la palabra dependerá de las letras seleccionadas."
    putStrLn "Las letras del tablero y de la mano se pueden usar una sola vez."
    putStrLn "Puede que la palabra formada no sea válida, en ese caso, intente de nuevo."
    putStrLn "Pulse cualquier tecla para volver al menú principal. ¡Buena suerte!"
    putStrLn ""
    _ <- getLine
    main

main :: IO ()
main = do
    putStrLn "Bienvenido a Mini-Scrabble"
    putStrLn "¿Qué desea hacer?"
    putStrLn "1. Jugar"
    putStrLn "2. ¿Cómo se juega?"
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
