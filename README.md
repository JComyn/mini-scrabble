# mini-scrabble

Este es un proyecto para la asignatura de Programación Funcional de la Universidad Politécnica de Madrid (UPM). El objetivo es simular una jugada del juego Scrabble, generando y filtrando palabras en castellano basadas en un conjunto de reglas.

## Descripción del Proyecto

### Jugando con las palabras

La mayoría de los pasatiempos típicos de los periódicos o los juegos de mesa de palabras se basan en manejar palabras. Este proyecto simula una jugada del conocido juego del Scrabble en castellano, estableciendo un conjunto de reglas para decidir cuándo una palabra es aceptable en castellano, incluso si no tiene significado. Dadas unas letras, se generan todas las posibles combinaciones posibles, y a continuación se les aplican una serie de filtros que descartan las que no podrían existir en español.

### Reglas de Validación

Algunas reglas básicas incluyen:
- Se define una distancia mínima (4 letras). Aunque pueden existir palabras de dos y tres letras, las descartamos al no ser buenas jugadas en el Scrabble, ya que siempre buscamos las más largas posibles con las letras disponibles.
-	No puede haber dos vocales iguales seguidas ni 3 vocales (no necesariamente iguales seguidas). Existen algunas excepciones, las más comunes se tienen en cuenta.
-	Reglas gramaticales sencillas.
-	No puede haber tres consonantes seguidas, existen excepciones.
-	Hay en ciertas letras en las que no acaba nunca una palabra (salvo muy contadas excepciones), como la H, K, V, W…
-	Ninguna palabra termina en dos consonantes. Las excepciones que existen son mayoritariamente anglicismos, se tienen en cuenta algunos de ellos (iceberg, test…)
-	Las palabras solo pueden empezar por una vocal, una consonante seguida de una vocal, o ciertas parejas de dos consonantes (bl, cr, dr, fl, fr, gr, pl, pr, tr, tl, ch, ll...)
-	Reglas sencillas de las sílabas de las palabras. Se basan en las vocales, las consonantes y ciertas parejas de consonantes permitidas.


