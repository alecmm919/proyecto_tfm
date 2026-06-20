# Título: 15_analisis_adicionales.R
#
# Autor: Alejandro M.
#
# Descripción: Este 'script' tiene como objetivo realizar pruebas sobre los resultados del proyecto para su inclusión en la memoria con rigor estadístico.

# Librerías y carga:
source("scripts/00_funciones.R")
library(stats)
library(rstatix)
library(tidyverse)
library(car)

# Sacamos los datos.
datos_20_4 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n20_g4.csv")
datos_20_5 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n20_g5.csv")
datos_20_6 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n20_g6.csv")
datos_30_4 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n30_g4.csv")
datos_30_5 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n30_g5.csv")
datos_30_6 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n30_g6.csv")
datos_40_4 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n40_g4.csv")
datos_40_5 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n40_g5.csv")
datos_40_6 <- read.csv("resultados/resultados_finales/repeticiones/14_aciertos_n40_g6.csv")

# Sacamos la tabla.
tabla <- rbind(datos_20_4, datos_30_4, datos_40_4, datos_20_5, datos_30_5, datos_40_5, datos_20_6, datos_30_6, datos_40_6)
tabla$aciertos <- as.numeric(tabla$aciertos)

# Obtenemos las probabilidades.
tabla[1:12,]$aciertos <- tabla[1:12,]$aciertos/3000 # Máximos aciertos para k = 4.
tabla[13:24,]$aciertos <- tabla[13:24,]$aciertos/5000 # Máximos aciertos para k = 5.
tabla[25:36,]$aciertos <- tabla[25:36,]$aciertos/7500 # Máximos aciertos para k = 6.

# Análisis 1: n, k y la naturaleza de los grupos afectan a la probabilidad de acierto: ANOVA de tres vías.
# Comprobamos supuestos.
shapiro.test(tabla[tabla$grupo == 1,]$aciertos)
shapiro.test(tabla[tabla$grupo == 2,]$aciertos)
shapiro.test(tabla[tabla$grupo == 3,]$aciertos)
shapiro.test(tabla[tabla$grupo == 4,]$aciertos)

bartlett.test(aciertos ~ grupo, data = tabla)

# Las variables son normales, pero heterocedásticas: corrección HC3.

# Modificamos la tabla.
tabla_1 <- tabla
tabla_1$n[1:12] <- 20
tabla_1$n[c(13:24)] <- 30
tabla_1$n[c(25:36)] <- 40
tabla_1$k <- rep(c(4, 5, 6), times = 3, each = 4)
tabla_1$grupo <- factor(tabla_1$grupo)

Anova(lm(aciertos ~ n * k * grupo, data = tabla_1), white.adjust = "hc3")