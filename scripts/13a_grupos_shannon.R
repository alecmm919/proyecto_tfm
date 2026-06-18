# Título: 13a_grupos_shannon.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se repetirá lo mismo que en el 10a y 10b, pero con 3 grupos. La Entropía de Shannon depende de k, así que debe normalizarse. Eliminamos uno de los dos grupos más similares de cada caso.

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

caso_1 <- subset(caso_1, grupo != "2")

caso_2$X <- NULL
caso_2 <- caso_2 %>%
    mutate(
        grupo = ifelse(grupo == "E1794D", "1", ifelse(grupo == "Q1785H", "2", ifelse(grupo == "R1753T", "3", "4")))
    )

caso_2 <- subset(caso_2, grupo != "2")

caso_3$X <- NULL

caso_3 <- subset(caso_3, grupo != "3")

caso_4$X <- NULL
caso_4 <- caso_4 %>%
    mutate(
        grupo = ifelse(grupo == "GPR_0", "1", ifelse(grupo == "GPR_2", "2", ifelse(grupo == "GPR_3", "3", "4")))
    )

caso_4 <- subset(caso_4, grupo != "2")

# Ahora, repetimos 500 veces el mismo experimento: tomar valores pseudoaleatorios y hacer la predicción.
resultados <- data.frame(
    H <- numeric(),
    n_aciertos <- numeric(),
    caso <- character()
)


for (i in 1:500){
    vec <- c(sample(5:35, size = 12)) # Decidimos los 4 tamaños muestrales.
    
    # Caso 1.
    n_aciertos <- analizar_arboles_shannon(
        caso_1,
        vec[1:3],
        c("B", "I", "P"),
        "tukey",
        NULL
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "1",
            H = calcular_shannon(vec[1:3]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 2.
    n_aciertos <- analizar_arboles_shannon(
        caso_2,
        vec[4:6],
        c("E1794D", "R1753T", "V1804D"),
        "dunn",
        c(2, 3, 8)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "2",
            H = calcular_shannon(vec[4:6]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 3.
    n_aciertos <- analizar_arboles_shannon(
        caso_3,
        vec[7:9],
        c("1", "2", "4"),
        "games",
        c(2, 3, 7)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "3",
            H = calcular_shannon(vec[7:9]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 4.
    n_aciertos <- analizar_arboles_shannon(
        caso_4,
        vec[10:12],
        c("GPR_0", "GPR_3", "GPR_4"),
        "dunn",
        c(2, 3, 8)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "4",
            H = calcular_shannon(vec[10:12]),
            n_aciertos = n_aciertos
        )
    )
}

write_csv(resultados, file = "resultados/resultados_finales/shannon/13a_entropia_shannon_3g.csv")