# Título: 10b_regresion_shannon.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' realizaremos una regresión COM-Poisson para buscar relaciones entre H y la probabilidad de acierto.

# Librerías y carga:
source("scripts/00_funciones.R")

library(nnet)
library(tidyverse)
library(car)
library(lattice)
library(ggplot2)
library(MASS)
library(RVAideMemoire)

datos <- read_csv("resultados/resultados_finales/shannon/10a_entropia_shannon.csv")

# Al tratarse de una variable de conteo, utilizaremos la regresión COM-Poisson porque la media es mucho mayor que la varianza:
mean(datos[datos$caso == 1, ]$H)
mean(datos[datos$caso == 2, ]$H)
mean(datos[datos$caso == 3, ]$H)
mean(datos[datos$caso == 4, ]$H)
var(datos[datos$caso == 1, ]$H)
var(datos[datos$caso == 2, ]$H)
var(datos[datos$caso == 3, ]$H)
var(datos[datos$caso == 4, ]$H)

for (i in 1:4){
    
    datos_corte <- datos %>%
        filter(
            caso == i,
            !is.na(n_aciertos),
            !is.na(H),
            n_aciertos >= 0,
            n_aciertos <= 6
        )
    
    hacer_regresion_binomial(
        datos_regresion = datos_corte,
        max_aciertos = 6,
        titulo_grafico = paste0("Regresión CMP, caso ", i),
        salida = paste0(
            "resultados/resultados_finales/shannon/10b_regresion_binomial_0",
            i,
            ".png"
        )
    )
}

# Hacemos también una regresión global.
datos_global <- datos %>%
    filter(
        !is.na(n_aciertos),
        !is.na(H),
        n_aciertos >= 0,
        n_aciertos <= 6
    )

hacer_regresion_binomial(
    datos_regresion = datos_global,
    max_aciertos = 6,
    titulo_grafico = "Regresión CMP global",
    salida = "resultados/resultados_finales/shannon/10b_regresion_global.png",
    color_caso = TRUE
)

# Estudiamos la correlación.
for (i in 1:4){
    print(spearman.ci(datos[datos$caso == i, ]$H, datos[datos$caso == i, ]$n_aciertos, nrep = 1000, conf.level = 0.95))
}