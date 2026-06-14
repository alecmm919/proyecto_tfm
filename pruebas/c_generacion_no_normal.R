# Título: c_generacion_no_normal.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se generar una serie de datos que no sigan distribuciones normales.
#
# Notas:
# 1)

# Librerías y carga:
n = 2500
source("scripts/00_funciones.R")
library(tidyverse)

set.seed(123)

# Creamos una serie de datos pseudoaleatorios con una distribución uniforme.
sim_1 <- sample(1:1000, n, replace = TRUE)
png("pruebas/graficas/c_distribucion_uniforme.png")
hist(sim_1)
dev.off()

# Ahora, procedemos a generar simulaciones con distintas distribuciones.
sim_poi <- rpois(n, lambda = 3) # Poisson.
sim_bin <- rbinom(2, n, prob = 0.5) # Binomial en la que se tiran monedas n veces.
png("pruebas/graficas/c_distribucion_poisson.png")
hist(sim_poi)
dev.off()
png("pruebas/graficas/c_distribucion_bin.png")
hist(sim_bin)
dev.off()

# Generamos variables con una correlación conocida.
rho <- 0.3
x1  <- rnorm(n, mean = 0, sd = 1) # Independiente.
x2  <- rho * x1 + x1 * rnorm(n, mean = 0, sd = 1) # Dependiente.

cor(x1, x2) # Debería dar alrededor de 0.3.
cor.test(x1, x2, method = "spearman") # Porque no son casos paramétrcos.

combo <- combinar_simulaciones(c(x1, x2), c("independiente", "dependiente"), 2500)
histograma(combo, "pruebas/graficas/c_correlacion_hist.png", valor, medias)
comprobar_parametricidad(combo, "medias")

png("pruebas/graficas/c_correlacion.png")
plot(x1, x2)
dev.off()