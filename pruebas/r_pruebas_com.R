# Título: q_pruebas_com.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' se hará una prueba de una regresión COM.

# Librerías y carga:
source("scripts/00_funciones.R")

library(nnet)
library(tidyverse)
library(car)
library(lattice)
library(ggplot2)
library(MASS)
library(COMPoissonReg)

datos <- read_csv("resultados/resultados_finales/shannon/10a_entropia_shannon.csv")

datos <- datos[datos$caso == "3",]

fit <- glm.cmp(formula.lambda = n_aciertos ~ H, data = datos)
plot(datos$H, datos$n_aciertos,
     pch = 16,
     xlab = "H",
     ylab = "n_aciertos")

# Predicciones
pred <- predict(fit, type = "response")

ord <- order(datos$H)

lines(datos$H[ord], pred[ord], lwd = 2)