# Título: d_prueba_04.R [DESCARTADO]
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del GPR como predictor de la hepatitis B, siguiendo la Bibliografía y los casos que buscamos. En este caso, se trata de un contexto no normal y heterocedástico.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)
library(ks)
library(cramer)
n = 71 # Número de datos.

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
    mutate(
        valor = simular_normal_exacto(valor)
    ) %>%
    ungroup()

# Hacemos las comprobaciones: se mantiene la falta de parametricidad y los estadísticos.
comprobar_normalidad_cramer(simulados, "grupo", "valor")
leveneTest(valor ~ grupo, data=simulados)

estadisticos_real <- generar_estadisticos_np(datos_reales, "grupo")
estadisticos_sim <- generar_estadisticos_np(simulados, "grupo")

# Mostramos los estadísticos. Al no ser normales, no realizamos la prueba t.
estadisticos_real
estadisticos_sim