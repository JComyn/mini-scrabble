import Data.List (subsequences, permutations)

-- Genera todas las permutaciones de una cadena de caracteres.
allPermutations :: String -> [String]
allPermutations s = concatMap permutations (subsequences s)


combinaciones :: String -> [String]
combinaciones = concatMap (filter (not . null) . permutations) . subsequences