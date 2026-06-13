# Título: 12b_regresion_shannon_5g.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' realizaremos una regresión binomial para buscar relaciones entre H y la probabilidad de acierto, esta vez con 5 grupos. Como H depende de k, debe normalizarse.

# Librerías y carga:
source("scripts/00_funciones.R")

library(nnet)
library(tidyverse)
library(car)
library(lattice)
library(ggplot2)
library(MASS)

datos <- read_csv("resultados/resultados_finales/shannon/12a_entropia_shannon_5g.csv")

for (i in 1:4){
    
    datos_corte <- datos %>%
        filter(
            caso == i,
            !is.na(n_aciertos),
            !is.na(H),
            n_aciertos >= 0,
            n_aciertos <= 10
        )
    
    hacer_regresion_binomial(
        datos_regresion = datos_corte,
        max_aciertos = 10,
        titulo_grafico = paste0("Regresión binomial, caso ", i),
        salida = paste0(
            "resultados/resultados_finales/shannon/12b_regresion_binomial_5grupos_0",
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
        n_aciertos <= 10
    )

hacer_regresion_binomial(
    datos_regresion = datos_global,
    max_aciertos = 10,
    titulo_grafico = "Regresión binomial global",
    salida = "resultados/resultados_finales/shannon/12b_regresion_global_5grupos.png",
    color_caso = TRUE
)

# Estudiamos la correlación.
for (i in 1:4){
    print(cor.test(x = datos[datos$caso == i, ]$H, y = datos[datos$caso == i, ]$n_aciertos, method = "kendall"))
}