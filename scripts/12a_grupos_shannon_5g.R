# Título: 12a_grupos_shannon_5g.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se repetirá lo mismo que en el 10, pero con 5 grupos. La Entropía de Shannon depende de k, así que debe normalizarse.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)

caso_1 <- read.csv("datos/simulados/01e_corazon_6g_n40.csv")
caso_2 <- read.csv("datos/simulados/02e_brca_6g_n40.csv")
caso_3 <- read.csv("datos/simulados/03e_estres_6g_n40.csv")
caso_4 <- read.csv("datos/simulados/04e_hepatitis_6g_n40.csv")

# Igualamos los grupos para evitar errores.
caso_1$gravedad <- NULL
caso_1 <- caso_1 %>%
    mutate(
        grupo = ifelse(grupo == "B", "1", ifelse(grupo == "C", "2", ifelse(grupo == "I", "3", ifelse(grupo == "P", "4", "5"))))
    )

caso_2$X <- NULL
caso_2 <- caso_2 %>%
    mutate(
        grupo = ifelse(grupo == "E1794D", "1", ifelse(grupo == "Q1785H", "2", ifelse(grupo == "R1753T", "3", ifelse(grupo == "V1804D", "4", "5"))))
    )

caso_3$X <- NULL

caso_3 <- caso_3 %>%
    mutate(
        grupo = ifelse(grupo == "G5", "5", grupo)
    )

caso_4$X <- NULL
caso_4 <- caso_4 %>%
    mutate(
        grupo = ifelse(grupo == "GPR_0", "1", ifelse(grupo == "GPR_2", "2", ifelse(grupo == "GPR_3", "3", ifelse(grupo == "GPR_4", "4", "5"))))
    )

# Ahora, repetimos 100 veces el mismo experimento: tomar valores pseudoaleatorios y hacer la predicción.
resultados <- data.frame(
    H <- numeric(),
    n_aciertos <- numeric(),
    caso <- character()
)


for (i in 1:500){
    vec <- c(sample(5:35, size = 20)) # Decidimos los 6 tamaños muestrales. Tomamos hasta 20 datos para evitar grandes desbalanceos.
    
    # Caso 1.
    n_aciertos <- analizar_arboles_shannon(
        caso_1,
        vec[1:5],
        c("B", "C", "I", "P", "G5"),
        "tukey",
        NULL
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "1",
            H = calcular_shannon(vec[1:5]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 2.
    n_aciertos <- analizar_arboles_shannon(
        caso_2,
        vec[6:10],
        c("E1794D", "Q1785H", "R1753T", "V1804D", "G5"),
        "dunn",
        c(2, 3, 8)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "2",
            H = calcular_shannon(vec[6:10]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 3.
    n_aciertos <- analizar_arboles_shannon(
        caso_2,
        vec[11:15],
        c("1", "2", "3", "4", "G5"),
        "games",
        c(2, 3, 7)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "3",
            H = calcular_shannon(vec[11:15]),
            n_aciertos = n_aciertos
        )
    )
    
    # Caso 4.
    n_aciertos <- analizar_arboles_shannon(
        caso_4,
        vec[16:20],
        c("GPR_0", "GPR_2", "GPR_3", "GPR_4", "G5"),
        "dunn",
        c(2, 3, 8)
    )
    
    # Analizamos.
    resultados <- rbind(
        resultados,
        data.frame(
            caso = "4",
            H = calcular_shannon(vec[16:20]),
            n_aciertos = n_aciertos
        )
    )
}

write_csv(resultados, file = "resultados/resultados_finales/shannon/12a_entropia_shannon_5g.csv")