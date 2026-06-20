# Título: 13b_regresion_shannon.R
#
# Autor: Alejandro M.
#
# Descripción: En este 'script' realizaremos una regresión para buscar relaciones entre H y la probabilidad de acierto.

# Librerías y carga:
source("scripts/00_funciones.R")

library(nnet)
library(tidyverse)
library(car)
library(lattice)
library(ggplot2)
library(MASS)

datos <- read_csv("resultados/resultados_finales/shannon/13a_entropia_shannon_3g.csv")

for (i in 1:4){
    
    datos_corte <- datos %>%
        filter(
            caso == i,
            !is.na(n_aciertos),
            !is.na(H),
            n_aciertos >= 0,
            n_aciertos <= 3
        )
    
    hacer_regresion_binomial(
        datos_regresion = datos_corte,
        max_aciertos = 3,
        titulo_grafico = paste0("Regresión CMP, caso ", i),
        salida = paste0(
            "resultados/resultados_finales/shannon/13b_regresion_binomial_3grupos_0",
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
        n_aciertos <= 3
    )

hacer_regresion_binomial(
    datos_regresion = datos_global,
    max_aciertos = 3,
    titulo_grafico = "Regresión CMP global",
    salida = "resultados/resultados_finales/shannon/13b_regresion_global_3grupos.png",
    color_caso = TRUE
)

# Estudiamos la correlación.
for (i in 1:4){
    print(spearman.ci(datos[datos$caso == i, ]$H, datos[datos$caso == i, ]$n_aciertos, nrep = 1000, conf.level = 0.95))
}