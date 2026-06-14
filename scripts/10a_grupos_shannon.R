# Título: 10a_grupos_shannon.R
#
# Autor: Alejandro M.
#
# Descripción: En el 'script' 09, parece que el desbalanceo de los grupos afecta al rendimiento de los árboles en todos los casos. En este 'script' se pretende hacer un nuevo análisis en el que se buscará la probabilidad de acierto del árbol en función de la entropía de Shannon de los grupos. Como se va a intentar un número relativamente alto de intentos, no se representarán árboles con rpart.plot.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(rpart)

# Para contar con más margen, partimos de los datos de la etapa 3 con n = 40.
caso_1 <- read.csv("datos/simulados/01b_corazon_40.csv")
caso_2 <- read.csv("datos/simulados/02b_brca_40.csv")
caso_3 <- read.csv("datos/simulados/03b_estres_40.csv")
caso_4 <- read.csv("datos/simulados/04b_hepatitis_40.csv")

# Igualamos los grupos para evitar errores.
caso_1$gravedad <- NULL
caso_1 <- caso_1 %>%
    mutate(
        grupo = ifelse(grupo == "B", "1", ifelse(grupo == "C", "2", ifelse(grupo == "I", "3", "4")))
    )

caso_2$X <- NULL
caso_2 <- caso_2 %>%
    mutate(
        grupo = ifelse(grupo == "E1794D", "1", ifelse(grupo == "Q1785H", "2", ifelse(grupo == "R1753T", "3", "4")))
    )

caso_3$X <- NULL

caso_4$X <- NULL
caso_4 <- caso_4 %>%
    mutate(
        grupo = ifelse(grupo == "GPR_0", "1", ifelse(grupo == "GPR_2", "2", ifelse(grupo == "GPR_3", "3", "4")))
    )

# Ahora, repetimos 100 veces el mismo experimento: tomar valores pseudoaleatorios y hacer la predicción.
resultados <- data.frame(
    H <- numeric(),
    n_aciertos <- numeric(),
    caso <- character()
)


for (i in 1:500){
    vec <- c(sample(5:35, size = 16)) # Decidimos los 4 tamaños muestrales.
    
    # Caso 1.
    n_aciertos <- analizar_arboles_shannon(
        caso_1,
        vec[1:4],
        c("B", "C", "I", "P"),
        "tukey",
        NULL
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "1",
            H = calcular_shannon(vec[1:4]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 2.
    n_aciertos <- analizar_arboles_shannon(
        caso_2,
        vec[5:8],
        c("E1794D", "Q1785H", "R1753T", "V1804D"),
        "dunn",
        c(2, 3, 8)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "2",
            H = calcular_shannon(vec[5:8]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 3.
    n_aciertos <- analizar_arboles_shannon(
        caso_2,
        vec[9:12],
        c("1", "2", "3", "4"),
        "games",
        c(2, 3, 7)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "3",
            H = calcular_shannon(vec[9:12]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 4.
    n_aciertos <- analizar_arboles_shannon(
        caso_4,
        vec[13:16],
        c("GPR_0", "GPR_2", "GPR_3", "GPR_4"),
        "dunn",
        c(2, 3, 8)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "4",
            H = calcular_shannon(vec[13:16]),
            n_aciertos = n_aciertos
        )
    )
}

write_csv(resultados, file = "resultados/resultados_finales/shannon/10a_entropia_shannon.csv")