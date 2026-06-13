# Título: 04b_simular_datos.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del GPR como predictor de la hepatitis B. Se generan 4 grupos de n = 20, n = 30 y n = 40 usando la función kde.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)
library(ks)

# Cargamos los datos y convertimos en factores.
datos_reales <- read_csv("datos/reales/04a_hepatitis_tidy.csv")
estadisticos_real <- read_csv("datos/reales/estadisticos/04a_estadisticos.csv")

estadisticos_real <- estadisticos_real %>%
    mutate(
        grupo = factor(grupo)
    )

datos_reales <- datos_reales %>%
    mutate(
        grupo = factor(grupo)
    )

# Creamos un 'tibble' para meter los datos. Usaremos una función ks, se trata de una función de densidad calculada en base a los datos originales y replicada en la simulación.
for (n in c(20, 30, 40)){
    simulados <- datos_reales %>%
        group_by(grupo) %>%
        group_modify(~ {
            x <- .x$valor
            dens <- kde(x)
            data.frame(
                valor = rkde(dens, n = n) # Esta función calcula la distribución.
            )
        }) %>%
        ungroup()
    
    simulados$grupo <- factor(simulados$grupo, levels = levels(datos_reales$grupo))
    comprobaciones_04(simulados, datos_reales, paste0("datos/simulados/04b_hepatitis_", n, ".csv"), paste0("datos/simulados/estadisticos/04b_hepatitis_", n, ".csv"))
}