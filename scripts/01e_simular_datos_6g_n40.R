# Título: 01e_simular_datos_6g_n40.R
#
# Autor: Alejandro M.
#
# Descripción: El objetivo de este 'script' es generar datos de la misma forma que en el 'script' 01c, pero con un valor de n = 40.

# Librerías y carga:
source("scripts/00_funciones.R")

n <- 40 # General para todos los grupos.

library(tidyverse)

datos_reales <- read.csv("datos/reales/01a_corazon_tidy.csv")
estadisticos_real <- read.csv("datos/reales/estadisticos/01a_estadisticos.csv")

estadisticos_real %>%
    mutate(
        grupo = factor(grupo)
    )

simulados <- datos_reales %>%
    group_by(grupo) %>%
    reframe(
        valor = simular_normal_exacto(valor, n)
    )

# Creamos el grupo 5.
filas <- nrow(datos_reales)
filas_simulados <- nrow(simulados)
for (i in 1:n){
    simulados[i+filas_simulados, ] <- list("G5", 0)
    datos_reales[i+filas, ] <- list("G5", 0)
}

# Creamos el grupoo 6.
filas <- nrow(datos_reales)
filas_simulados <- nrow(simulados)
for (i in 1:n){
    simulados[i+filas_simulados, ] <- list("G6", 0)
    datos_reales[i+filas, ] <- list("G6", 0)
}

# Sacamos estadísticos.
media <- mean(estadisticos_real$media)
dest <- mean(estadisticos_real$dest)

# Simulamos el nuevo grupo.
simulados[simulados$grupo == "G5", ] <- datos_reales[datos_reales$grupo == "G5", ] %>%
    reframe(
        grupo = "G5",
        valor = simular_normal_exacto(valor, n, mu = media, s = dest) # Simulamos un grupo 'promedio' en sus dos estadísticos.
    )

simulados[simulados$grupo == "G6", ] <- datos_reales[datos_reales$grupo == "G6", ] %>%
    reframe(
        grupo = "G6",
        valor = simular_normal_exacto(valor, n, mu = mean(c(estadisticos_real[2,2], estadisticos_real[4,2])), s = dest) # Simulamos un grupo suma de otros dos. Debe ser homocedástico al resto.
    )

# Pasamos a factores.
simulados$gravedad <- factor(simulados$grupo)

# Guardamos.
comprobaciones_01(simulados, "datos/simulados/01e_corazon_6g_n40.csv", "datos/simulados/estadisticos/01e_corazon_6g_n40.csv")