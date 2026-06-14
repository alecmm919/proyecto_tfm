# Título: b_generacion_normalhetero.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se pretende generar una serie de datos que sean normales, pero heterocedásticos.
#
# Notas:
# 1)

# Librerías y carga:
n = 2500
source("scripts/00_funciones.R")
library(tidyverse)

set.seed(123)

# Simulación de muestras normales y heterocedásticas
sim_1 <- rnorm(n, mean = 1, sd = 1)
sim_3 <- rnorm(n, mean = 3, sd = 3)
sim_5 <- rnorm(n, mean = 5, sd = 5)
sim_10 <- rnorm(n, mean = 10, sd = 10)
sim_20 <- rnorm(n, mean = 20, sd = 20)

# Combinamos las simulaciones en un solo 'data frame'.
combo_het <- combinar_simulaciones(c(sim_1, sim_3, sim_5, sim_10, sim_20), c("1", "3", "5", "10", "20"), 2500)

# Sacamos el histograma.
histograma(combo_het, "pruebas/graficas/b_heterocedastico.png", valor, medias)

# Comprobamos la normalidad de las simulaciones.
comprobar_parametricidad(combo_het, "medias")