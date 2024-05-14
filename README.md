# mini-scrabble

Este es un proyecto para la asignatura de Programación Funcional de la Universidad Politécnica de Madrid (UPM). El objetivo es simular una jugada del juego Scrabble, generando y filtrando palabras en castellano basadas en un conjunto de reglas.

## Descripción del Proyecto

### Jugando con las palabras

La mayoría de los pasatiempos típicos de los periódicos o los juegos de mesa de palabras se basan en manejar palabras. Este proyecto simula una jugada del conocido juego del Scrabble en castellano, estableciendo un conjunto de reglas para decidir cuándo una palabra es aceptable en castellano, incluso si no tiene significado.

### Reglas de Validación

Algunas reglas básicas incluyen:
- Las consonantes que pueden aparecer al final de una sílaba son L, N, S, R.
- Solo aparecen dos consonantes al inicio de una sílaba si la segunda es una L o R.
- La CH es considerada una única letra.
- Tres consonantes pueden aparecer seguidas solo en casos específicos, como una final de sílaba seguida de un grupo doble al inicio de la siguiente sílaba.
- La N se convierte en M antes de B o P.

