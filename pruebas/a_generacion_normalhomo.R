# Título: a_generacion_normalhomo.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se pretende generar una serie de datos que cumplan los supuestos paramétricos, es decir, que sean normales y homocedásticos. Para ello, se usará la simulación Montecarlo.
#

# Librerías y carga:
source("scripts/00_funciones.R")
library(graphics)
library(tidyverse)

# Simulación simple de datos normales estándares.
n <- 2500
sim <- rnorm(n, mean = 0, sd = 1)
plot(sim)

# Sacamos el histograma.
png(filename = "pruebas/graficas/a_simplenormal.png")
hist(sim, main = "Histograma simple", xlab = "Marcas de clase", ylab = "Frecuencia")
dev.off()

# Simulación de muestras normales y homocedásticas.
dest <- 2
sim_2 <- rnorm(n, mean = 2, sd = dest)
sim_4 <- rnorm(n, mean = 4, sd = dest)
sim_6 <- rnorm(n, mean = 6, sd = dest)
sim_8 <- rnorm(n, mean = 8, sd = dest)
sim_10 <- rnorm(n, mean = 10, sd = dest)

# Combinamos las simulaciones en un solo 'data frame'.
combo_df <- combinar_simulaciones(c(sim_2, sim_4, sim_6, sim_8, sim_10), c("2", "4", "6", "8", "10"), 2500)

# Sacamos el histograma.
histograma(combo_df, "pruebas/graficas/a_compuestonormal.png", valor, medias)

# Comprobamos la normalidad de las simulaciones.
comprobar_parametricidad(combo_df, "medias")