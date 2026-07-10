# Proyecto: Comparación entre técnicas de estadística clásica y de *machine learning* en entornos bioinformáticos.

Este repositorio se ha elaborado y publicado con el *único* propósito de su evaluación académica.

## Lenguajes usados: ```R v4.5.3``` (100%).
Para mayor información sobre las librerías cargadas, consultar ```sessionInfo.txt```.

# Estructura del proyecto

## *Script* ```PIPELINE_COMPLETO.R```

Contiene todo el trabajo en un solo *script.*

## Directorio de pruebas

Contiene pruebas auxiliares realizadas a lo largo del proyecto y *no deben considerarse* como parte del mismo.

## Directorio de resultados
Los resultados de este trabajo se dividen en dos grupos: exploratorios y finales. Los primeros buscan explorar las técnicas estadísticas y los árboles de decisión. Los otros, los finales, aumentan el número de repeticiones a 500 para buscar mayor potencia.

### Resultados exploratorios

- analisis_aciertos contiene los aciertos de las técnicas de aprendizaje automático repitiendo una vez el experimento.
- arboles contiene los árboles de decisión y las matrices de confusión.
- comparaciones_clasicas_ad contiene las separaciones de los grupos mediante estadística clásica.
- tablas_comparativas muestra tablas en las que se comparan los aciertos y errores de separación de los árboles de decisión en comparación con la estadística clásica.
- informes_comparativos contiene los informes que demuestran que los datos simulados mantienen las propiedades estadísticas de los reales.

### Resultados finales

- repeticiones contiene los resultados con los experimentos repetidos 500 veces.
- shannon contiene los resultados de las regresiones CMP, también repetidas 500 veces.