# Título: 04d_simular_datos_desbalanceados.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del GPR como predictor de la hepatitis B. En este caso, los grupos tienen distinto número de datos. Como se trata de una exploración, los grupos se han desbalanceado arbitrariamente.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)
library(ks)

# Generamos números de datos pseudoaleatorios.
var <- c(20, 25, 30, 15)
names(var) <- c("GPR_0", "GPR_2", "GPR_3", "GPR_4")
n <- c(var["GPR_0"], var["GPR_2"], var["GPR_3"], var["GPR_4"])

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
simulados <- datos_reales %>%
    group_by(grupo) %>%
    group_modify(~ {
        x <- .x$valor
        g <- .y$grupo
        dens <- kde(x)
        data.frame(
            valor = rkde(dens, n = n[as.character(g)])
        )
    }) %>%
    ungroup()

simulados$grupo <- factor(simulados$grupo, levels = levels(datos_reales$grupo))

# Hacemos las comprobaciones: se mantiene la falta de parametricidad y los estadísticos.
comprobaciones_04(simulados, datos_reales, "datos/simulados/04d_hepatitis_desbalanceados.csv", "datos/simulados/estadisticos/04d_hepatitis_desbalanceados.csv")