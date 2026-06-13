# Título: 03c_simular_datos_6g_n20.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos para la simulación del contexto biológico del estrés en ratas. Se generan dos grupos extra bajo los mismos criterios que en el 'script' 01c.

# Librerías y carga:
source("scripts/00_funciones.R")
library(tidyverse)
library(car)

n <- 20 # Número de datos.

# Cargamos los datos y convertimos en factores.
datos_reales <- read_csv("datos/reales/03a_estres_tidy.csv")

estadisticos_real <- read_csv("datos/reales/estadisticos/03a_estadisticos.csv")

datos_reales$grupo <- as.character(datos_reales$grupo)

# Generamos dos grupos extra.
filas <- nrow(datos_reales)
for (i in 1:n){
    datos_reales[i+filas, ] <- list("G5", 0)
}

filas <- nrow(datos_reales)
for (i in 1:n){
    datos_reales[i+filas, ] <- list("G6", 0)
}

# Simulamos los grupos por sus distintos estadísticos.
simulados <- datos_reales %>%
    group_by(grupo) %>%
    reframe(
        valor = simular_normal_exacto(valor, n)
    )

# Simulamos los grupos nuevos.
media <- mean(datos_reales$valor, na.rm = TRUE)

# Simulamos los nuevos grupos usando los estadísticos.
simulados[simulados$grupo == "G5", ] <- datos_reales[datos_reales$grupo == "G5", ] %>%
    reframe(
        grupo = "G5",
        valor = simular_normal_exacto(valor, n, mu = media, s = mean(c(as.numeric(estadisticos_real[1,3]), as.numeric(estadisticos_real[2,3]), as.numeric(estadisticos_real[3,3]), as.numeric(estadisticos_real[4,3])))) # Simulamos un grupo 'promedio' en sus dos estadísticos.
    )

simulados[simulados$grupo == "G6", ] <- datos_reales[datos_reales$grupo == "G6", ] %>%
    reframe(
        grupo = "G6",
        valor = simular_normal_exacto(valor, n, mu = as.numeric(estadisticos_real[2,2]) + as.numeric(estadisticos_real[3,2]), s = mean(c(as.numeric(estadisticos_real[3,2]), as.numeric(estadisticos_real[3,3])))) # Simulamos la suma de dos grupos.
    )

# Hacemos las comprobaciones: se mantiene la falta de parametricidad y los estadísticos.
simulados$grupo <- factor(simulados$grupo)
comprobaciones_03(simulados, datos_reales, "datos/simulados/03c_estres_6g_n20.csv", "datos/simulados/estadisticos/03c_estres_6g_n20.csv")