# Título: 03d_simular_datos_desbalanceados.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del estrés en ratas. En este caso, los grupos tienen distinto número de datos. Estos grupos desbalanceados se utilizan únicamente con fines exploratorios, por lo que se han desbalanceado arbitrariamente.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)

# Generamos grupos desbalanceados
n <- c(25, 30, 15, 20)

# Cargamos los datos y convertimos en factores.
datos_reales <- read_csv("datos/reales/03a_estres_tidy.csv") %>%
    mutate(
        grupo = factor(grupo)
    )

estadisticos_real <- read_csv("datos/reales/estadisticos/03a_estadisticos.csv")

# Simulamos los grupos por sus distintos estadísticos.
simulados <- datos_reales %>%
    group_by(grupo) %>%
    reframe(
        valor = simular_normal_exacto(valor, n[first(grupo)])
    )

# Hacemos las comprobaciones: se mantiene la falta de parametricidad y los estadísticos.
simulados$grupo <- factor(simulados$grupo)
comprobaciones_03(simulados, datos_reales, "datos/simulados/03d_estres_desbalanceados.csv", "datos/simulados/estadisticos/03d_estres_desbalanceados.csv")