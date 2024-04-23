---
title: Laboratorio de Funcional
author: acá van sus nombres
---
La consigna del laboratorio está en https://tinyurl.com/funcional-2024-famaf

# 1. Tareas
Pueden usar esta checklist para indicar el avance.

## Verificación de que pueden hacer las cosas.
- [ ] Haskell instalado y testeos provistos funcionando. (En Install.md están las instrucciones para instalar.)

## 1.1. Lenguaje
- [ ] Módulo `Dibujo.hs` con el tipo `Dibujo` y combinadores. Puntos 1 a 3 de la consigna.
- [ ] Definición de funciones (esquemas) para la manipulación de dibujos.
- [ ] Módulo `Pred.hs`. Punto extra si definen predicados para transformaciones innecesarias (por ejemplo, espejar dos veces es la identidad).

## 1.2. Interpretación geométrica
- [ ] Módulo `Interp.hs`.

## 1.3. Expresión artística (Utilizar el lenguaje)
- [ ] El dibujo de `Dibujos/Feo.hs` se ve lindo.
- [ ] Módulo `Dibujos/Grilla.hs`.
- [ ] Módulo `Dibujos/Escher.hs`.
- [ ] Listado de dibujos en `Main.hs`.

## 1.4 Tests
- [ ] Tests para `Dibujo.hs`.
- [ ] Tests para `Pred.hs`.

# 2. Experiencia
Completar

# 3. Preguntas
Al responder tranformar cada pregunta en una subsección para que sea más fácil de leer.

1. ¿Por qué están separadas las funcionalidades en los módulos indicados? Explicar detalladamente la responsabilidad de cada módulo.
    La idea de separar en modulos diferentes es centrarnos en crear un lenguaje y luego interpretarlo. 
    El modulo Dibujo.hs es quien se encarga de crear y modelar este lenguaje, en la respuesta 2 tambien veremos mas ventajas al respecto de esto, pero debemos notar que no se hace uso de gloss en particular en este modulo es decir no es la idea representar las figuras geometricas aqui solo definir el lenguaje.
    El modulo Interp.hs si hace uso de gloss y es por eso que es el encargado de interpretar el lenguaje para que las figuras geometricas puedan aparecen en pantalla 

2. ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez de eso, es un parámetro del tipo?
    Los parámetros de tipo se usan para poder cambiar la figura en general y poder usar el mismo codigo para hacer distintas figuras es otra ventaja de separar en módulos distintos, concretamente podemos operar con ellas independientemente de su interpretacion

3. ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?
    Una de las cosas a notar es que Fold hace que el programa sea mas expresivo sobre lo que hace y no sobre como lo hace (es mas declarativo)
    Fold tambien hace que el codigo sea mas generico y es por eso que la podemos utilizar en muchísimas funciones y no solo unas pocas veces
    aunque tambien debemos notar ciertas desventajas en algunas circunstancias por ejemplo 
    Hacer pattern-matching otra un mejor control sobre la ejecución que queremos hacer 
    en casos mas sencillos por ejemplo acceder al primer elemento de una array es mas sencillo utilizar pattern-matching

4. ¿Cuál es la diferencia entre los predicados definidos en Pred.hs y los tests?
    Los predicados de Pred.hs estan hechos para ser utilizados por otras función, aunque en particular este grupo no dio uso a Pred.hs entendemos que se la idea es utilizar sus predicados para hacer operaciones especificas.
    En cambio los predicados de los test estan hechos exclusivamente para de Dibujo.hs y Pred.hs funcionen como es debido 

# 4. Extras
Completar si hacen algo.