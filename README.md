# Proyecto: Comparación entre técnicas de estadística clásica y de *machine learning* en entornos bioinformáticos.

# *Scripts*

Todos los *scripts* comienzan con una breve descripción de su objetivo. Los *scripts* 01 - 09 tienen objetivos exploratorios. A partir del *script* 10, se realizan las pruebas con un mayor número de repeticiones para buscar patrones más fiables.

Aquí se hace un breve resumen de lo que hace cada *script.*

## 00_funciones.R
Contiene todas las funciones utilizadas a lo largo del proyecto y la semilla de reproducibilidad. Este *script* se carga al principio de cada uno de los demás:

```{r}
source("scripts/00_funciones.R")
```

Esto se realiza así para garantizar la reproducibilidad y la carga de funciones independientemente del orden de ejecución.

## *Scripts* 01, 02, 03 y 04
Preparan los datos reales y simulan a partir de estos. Los números 01, 02, 03 y 04 se corresponden con los cuatro casos biológicos de estudio (paramétrico, normal heterocedástico, no normal homocedástico y no normal heterocedástico respectivamente). En todos se comprueba que respetan los mismos supuestos estadísticos que los reales. Estos *scripts* se dividen en:

    a) Preparar y limpiar los datos.
    b) Simular los datos con $n=20$, $n=30$, $n=40$ datos y comprobar que siguen respetando los estadísticos reales.
    c) Simular con dos grupos extra, estos dos grupos (G5 y G6) *siempre* se generan de la misma forma: G5 es la media de todos los grupos y G6 es la media de los dos grupos con menor media. En este caso, con $n=20$.
    d) Simular datos desbalanceados.
    e) Simular nuevamente datos desbalanceados, como en el punto c), pero con $n=40$.

## *Script* 05
Compara los estadísticos de los grupos reales y simulados. En los casos normales, hace pruebas $t$ y $F$ para contrastar la igualdad de medias y varianzas. En los no normales, se realiza una prueba $U$ de Mann-Whitney para contrastar igualdad de la forma de las distribuciones. En todos los casos, se ajustan los $p$ valores con el método de Holm.

## *Script* 06
Busca diferencias entre grupos mediante un ANOVA (o una prueba no paramétrica, según el caso), con su correspondiente *post hoc.*

## *Script* 07
Genera los árboles de decisión.

## *Script* 08
Compara las separaciones hechas por estadística clásica y por árboles de decisión.

## *Script* 09
Analiza y contabiliza los fallos y aciertos cometidos por los árboles en función de las características de los datos.

## *Script* 10
A partir de este momento, se ha llegado a dos aparentes conclusiones:
    
    1) El valor de $k$ afecta a la probabilidad de acierto.
    2) La parametricidad o ausencia de ella también afecta a la probabilidad de acierto.
    
Por ello, en este *script,* se generan 500 veces grupos desbalanceados, se analizan de forma similar al *script* 09, pero teniendo en cuenta la Entropía de Shannon y normalizándola. Posteriormente, se hace una regresión binomial. Al tratarse de una regresión que cuenta el número de aciertos, no normalizaremos esta segunda variable discreta.

## *Script* 11
Hace lo mismo que el *script* 10, pero con $k = 6$.

## *Script* 12
Hace lo mismo que el *script* 10, pero con $k = 5$.

## *Script* 13
Repite los análisis de los *scripts* 1, 2, 3, 4, 8 y 9 pero con 500 repeticiones para ganar potencia. Es decir, se toman datos, se separan por estadística clásica y por árboles de decisión y se calculan los aciertos.

## 99_correr_todo.R
Ejecuta todos los *scripts* en orden y actualiza el archivo sessionInfo.txt. Nótese que este archivo *solo* se actualiza tras la ejecución de este *script.*

# Directorio de resultados
Los resultados de este trabajo se dividen en dos grupos: exploratorios y finales. Los exploratorios son los resultados de los *scripts* 01 - 09 y los finales, del 10 en adelante. Estos últimos aumentan el número de repeticiones a 500 para buscar mayor potencia.

    1) analisis_aciertos contiene las salidas del *script* 09, que cuenta los aciertos de las técnicas de aprendizaje automático repitiendo una vez el experimento.
    2) arboles contiene los árboles de decisión y las matrices de confusión.
    3) comparaciones_clasicas_ad contiene las separaciones de los grupos mediante estadística clásica.
    4) repeticiones contiene los resultados del *script* 13, con los experimentos repetidos 50 veces.
    5) shannon contiene los resultados de los *scripts* 10, 11 y 12, con las regresiones binomiales.
    6) tablas_comparativas muestra tablas en las que se comparan los aciertos y errores de separación de los árboles de decisión en comparación con la estadística clásica.